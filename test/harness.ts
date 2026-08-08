import * as Scheme from '../src/scheme'
import * as LPM from '../src/lpm'
import { diagnosticToError } from '../src/scheme/diagnostic'
import { Fiber } from '../src/lpm/fiber'
import { runFiberOnScheduler } from '../src/lpm/run'
import { makeTraceStepper } from '../src/scheme/trace'
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

/** A LoggingChannel that drops each reported error's range. See RunOptions. */
class StrippingChannel extends LPM.LoggingChannel {
  report (e: LPM.ScamperError): void {
    super.report(e.stripRange())
  }
}

function mkChannel (opts: RunOptions, renderOutput = true): LPM.LoggingChannel {
  return opts.stripRanges
    ? new StrippingChannel(renderOutput)
    : new LPM.LoggingChannel(renderOutput)
}

/**
 * Compiles and runs `src` on a real Scheduler.
 *
 * Scheduler-driven on purpose: it is the only thing that services blocking
 * primitives (`with-file`, the `file` library, `with-image-from-url`) and file
 * imports, so tests exercise the same execution path the IDE and CLI do. Use
 * this for any test about what a program *does*; drop to
 * `stepFiberToCompletion` (test/util.ts) only for LPM-level tests of the fiber
 * itself.
 *
 * @returns the program's displayed values and reported errors, in order.
 */
export async function runProgram (src: string, opts: RunOptions = {}): Promise<string[]> {
  src = src.trim()
  const out = mkChannel(opts)
  const { prog, diagnostics } = await Scheme.compile(src)
  diagnostics.forEach((d) => { out.report(diagnosticToError(d)) })
  if (out.log.length !== 0) { return out.log as string[] }
  if (prog === undefined) {
    throw new Error('compile produced no program and no logged errors')
  }
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
    out,
    err: out,
  })
  return out.log as string[]
}

/**
 * As {@link runProgram}, but returns each displayed value *unrendered*, for
 * tests that assert on the values themselves rather than on their text.
 * Reported errors still appear as their rendered strings, in order.
 */
export async function runProgramValues (
  src: string,
  opts: RunOptions = {},
): Promise<LPM.Value[]> {
  src = src.trim()
  const out = mkChannel(opts, false)
  const { prog, diagnostics } = await Scheme.compile(src)
  diagnostics.forEach((d) => { out.report(diagnosticToError(d)) })
  if (out.log.length !== 0) { return out.log }
  if (prog === undefined) {
    throw new Error('compile produced no program and no logged errors')
  }
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
    out,
    err: out,
  })
  return out.log
}

/**
 * As {@link runProgram}, but emits the program's reduction trace: an opening
 * line with the program's initial state, then each user-visible reduction
 * marked `--> `.
 */
export async function runProgramTraced (
  src: string,
  isTracing = true,
  opts: RunOptions = {},
): Promise<string[]> {
  src = src.trim()
  const out = mkChannel(opts)
  const { prog, diagnostics } = await Scheme.compile(src)
  diagnostics.forEach((d) => { out.report(diagnosticToError(d)) })
  if (out.log.length !== 0) { return out.log as string[] }
  if (prog === undefined) {
    throw new Error('compile produced no program and no logged errors')
  }
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
    out,
    err: out,
    isTracing,
    stepper: isTracing ? makeTraceStepper() : undefined,
  })
  return out.log as string[]
}

/**
 * The reduction trace of `src` as bare expressions, with the `--> ` reduction
 * marker stripped -- the shape trace tests assert on.
 */
export async function reductionTrace (src: string): Promise<string[]> {
  const log = await runProgramTraced(src)
  return log.map((l) => (l.startsWith('--> ') ? l.slice(4) : l))
}

export async function runProgramWithHTML (src: string, out: HTMLDisplay): Promise<HTMLElement[]> {
  src = src.trim()
  const { prog, diagnostics } = await Scheme.compile(src)
  diagnostics.forEach((d) => { out.report(diagnosticToError(d)) })

  if (out.levels.length > 1) { return out.levels }
  if (prog === undefined) {
    throw new Error('compile produced no program and no logged errors')
  }
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
    out,
    err: out,
  })
  return out.levels
}
