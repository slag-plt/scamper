import {expect, test} from 'vitest'

import builtinLibs from '../src/lib'
import * as Scheme from '../src/scheme'
import * as LPM from '../src/lpm'

function runProgram (src: string): string[] {
    const out = new LPM.LoggingChannel()
    const env = Scheme.mkInitialEnv()
    const prog = Scheme.compile(out, src)
    if (out.log.length !== 0) { return out.log }
    const machine = new LPM.Machine(
      builtinLibs,
      env,
      prog!,
      out,
      out
    )
    machine.evaluate()
    return out.log
}

export function scamperTest (label: string, src: string, expected: string[]) {
  test(label, () => expect(runProgram(src.trim())).toEqual(expected))
}

export function failingScamperTest (label: string, src: string, expected: string[]) {
  test.fails(label, () => expect(runProgram(src.trim())).toEqual(expected))
}