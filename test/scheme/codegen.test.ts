import { expect, test, describe } from "@jest/globals"

import builtinLibs from '../../src/lib'
import * as Scheme from '../../src/scheme'
import * as LPM from '../../src/lpm'

describe('Basic codegen', () => {
  test('simple arithmetic', () => {
    const out = new LPM.LoggingOutputChannel() 
    const err = new LPM.LoggingErrorChannel()
    const env = Scheme.mkInitialEnv()
    const prog = Scheme.compile(err, '(display (+ 1 1))')
    expect(err.log).toEqual([])
    const machine = new LPM.Machine(
      builtinLibs,
      env,
      prog!,
      out,
      err
    )
    machine.evaluate()
    expect(out.log).toEqual([2])
  })
})