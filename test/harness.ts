import * as Scheme from '../src/scheme'
import * as LPM from '../src/lpm'
import { diagnosticToError } from '../src/scheme/diagnostic'
import { Fiber } from '../src/lpm/fiber'
import HTMLDisplay from '../src/lpm/output/html'

/** Options controlling how {@link runProgram} reports errors. */
export interface RunOptions {
  /**
   * Drop each reported error's source range before logging it, so the output
   * lines omit their `[line:col-...]` location. Library tests set this so they
   * can assert on error *messages* without coupling to library line numbers
   * (contract errors point at the offending definition in the `.scm` source,
   * so any library edit would otherwise shift these ranges). Range *reporting*
   * itself is covered directly in test/lpm/range.test.ts.
   */
  stripRanges?: boolean
}

/** Reports `e` to `out`, stripping its range first when `stripRanges` is set. */
function reportError (
  out: LPM.ErrorChannel,
  e: LPM.ScamperError,
  stripRanges: boolean,
) {
  out.report(stripRanges ? e.stripRange() : e)
}

// Runs a fiber to completion, sending displayed values/reported errors to
// `out` and recovering from runtime errors the way the scheduler does for
// display tasks: report the error, then move on to the next statement.
function runFiber (
  fiber: Fiber,
  out: LPM.OutputChannel & LPM.ErrorChannel,
  stripRanges = false,
) {
  while (!fiber.isDone()) {
    try {
      const res = fiber.step()
      if (res.tag === 'display') {
        out.send(fiber.lastResult)
      }
    } catch (e) {
      if (e instanceof LPM.ScamperError) {
        // Mirror the scheduler: give an installed with-handler a chance to
        // recover before reporting and unwinding to the next statement.
        if (!(e instanceof LPM.ReportError) && fiber.handleError(e)) {
          continue
        }
        reportError(out, e, stripRanges)
        fiber.advanceStmt()
      } else {
        throw e
      }
    }
  }
}

export async function runProgram (src: string, opts: RunOptions = {}): Promise<string[]> {
    src = src.trim()
    const out = new LPM.LoggingChannel()
    const env = Scheme.mkInitialEnv()
    const { prog, diagnostics } = await Scheme.compile(src)
    diagnostics.forEach((d) => { reportError(out, diagnosticToError(d), opts.stripRanges ?? false) })
    if (out.log.length !== 0) { return out.log as string[] }
    if (prog === undefined) {
      throw new Error('compile produced no program and no logged errors')
    }
    const fiber = new Fiber(prog, env)
    runFiber(fiber, out, opts.stripRanges)
    return out.log as string[]
}

export async function runProgramWithHTML (src: string, out: HTMLDisplay): Promise<HTMLElement[]> {
    src = src.trim()
    const env = Scheme.mkInitialEnv()
    const { prog, diagnostics } = await Scheme.compile(src)
    diagnostics.forEach((d) => { out.report(diagnosticToError(d)) })

    if (out.levels.length > 1) { return out.levels }
    if (prog === undefined) {
      throw new Error('compile produced no program and no logged errors')
    }
    const fiber = new Fiber(prog, env)
    runFiber(fiber, out)
    return out.levels
}
