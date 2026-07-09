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

export async function runProgram (src: string): Promise<string[]> {
    src = src.trim()
    const out = new LPM.LoggingChannel()
    const env = Scheme.mkInitialEnv()
    const prog = await Scheme.compile(out, src)
    if (out.log.length !== 0) { return out.log as string[] }
    if (prog === undefined) {
      throw new Error("compile produced no program and no logged errors")
    }
    const fiber = new Fiber(prog, env)
    runFiber(fiber, out)
    return out.log as string[]
}

export async function runProgramWithHTML (src: string, out: HTMLDisplay): Promise<HTMLElement[]> {
    src = src.trim()
    const env = Scheme.mkInitialEnv()
    const prog = await Scheme.compile(out, src)

    if (out.levels.length > 1) { return out.levels }
    if (prog === undefined) {
      throw new Error("compile produced no program and no logged errors")
    }
    const fiber = new Fiber(prog, env)
    runFiber(fiber, out)
    return out.levels
}

export function scamperTest (label: string, src: string, expected: string[]) {
  test(label, async () => { expect(await runProgram(src.trim())).toEqual(expected); })
}

export function failingScamperTest (label: string, src: string, expected: string[]) {
  test.fails(label, async () => { expect(await runProgram(src.trim())).toEqual(expected); })
}
