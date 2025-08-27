import { expect, test, describe } from "@jest/globals"

import builtinLibs from '../../src/lib'
import * as Scheme from '../../src/scheme'
import * as LPM from '../../src/lpm'

function checkMachineOutput (src: string, expected: LPM.Value[]) {
    const out = new LPM.LoggingOutputChannel() 
    const err = new LPM.LoggingErrorChannel()
    const env = Scheme.mkInitialEnv()
    const prog = Scheme.compile(err, src)
    expect(err.log).toEqual([])
    const machine = new LPM.Machine(
      builtinLibs,
      env,
      prog!,
      out,
      err
    )
    machine.evaluate()
    expect(out.log).toEqual(expected)
}


describe('Basic codegen', () => {
  test('simple arithmetic', () => {
    checkMachineOutput(`
      (display (+ 1 1))  
    `, [2])
  })
})

describe('End-to-end cases', () => {
  test('factorial', () => {
    checkMachineOutput(`
      (define fact
        (lambda (n)
          (if (zero? n)
              1
              (* n (fact (- n 1))))))

      (display (fact 5))
    `, [120])
  })

  test('basic list operations', () => {
    checkMachineOutput(`
      (define list-length
        (lambda (l)
          (if (null? l)
              0
              (+ 1 (list-length (cdr l))))))
      (display (list-length '()))
    `, [0])
  })

  test('basic struct operations', () => {
    checkMachineOutput(`
      (struct point (x y))
      (define p (point 1 2))
      (display (point-x p))
    `, [1])
  })
})