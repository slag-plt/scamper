import { expect, test } from 'vitest'
import * as Scheme from '../src/scheme'
import * as LPM from '../src/lpm'
import { Fiber } from '../src/lpm/fiber'
import HTMLDisplay from "../src/lpm/output/html";

// Runs a fiber to completion, sending displayed values/reported errors to
// `out` and recovering from runtime errors the way the scheduler does for
// display tasks: report the error, then move on to the next statement.
function runFiber (fiber: Fiber, out: LPM.OutputChannel & LPM.ErrorChannel) {
  while (!fiber.isDone()) {
    try {
      const res = fiber.step()
      if (res.tag === 'display') {
        out.send(fiber.lastResult)
      }
    } catch (e) {
      if (e instanceof LPM.ScamperError) {
        out.report(e)
        fiber.advanceStmt()
      } else {
        throw e
      }
    }
  }
}

export function runProgram (src: string): string[] {
    src = src.trim()
    const out = new LPM.LoggingChannel()
    const env = Scheme.mkInitialEnv()
    const prog = Scheme.compile(out, src)
    if (out.log.length !== 0) { return out.log as string[] }
    const fiber = new Fiber(prog!, env)
    runFiber(fiber, out)
    return out.log as string[]
}

export function runProgramWithHTML (src: string, out: HTMLDisplay): HTMLElement[] {
    src = src.trim()
    const env = Scheme.mkInitialEnv()
    const prog = Scheme.compile(out, src)

    if (out.levels.length > 1) { return out.levels }
    const fiber = new Fiber(prog!, env)
    runFiber(fiber, out)
    return out.levels
}

export function scamperTest (label: string, src: string, expected: string[]) {
  test(label, () => { expect(runProgram(src.trim())).toEqual(expected); })
}

export function failingScamperTest (label: string, src: string, expected: string[]) {
  test.fails(label, () => { expect(runProgram(src.trim())).toEqual(expected); })
}