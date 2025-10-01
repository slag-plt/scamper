import { expect, test, describe } from 'vitest'

import builtinLibs from '../../src/lib'
import * as Scheme from '../../src/scheme'
import * as LPM from '../../src/lpm'

function checkMachineOutput (src: string, expected: LPM.Value[]) {
    const out = new LPM.LoggingChannel()
    const env = Scheme.mkInitialEnv()
    const prog = Scheme.compile(out, src)
    expect(out.log).toEqual([])
    const machine = new LPM.Machine(
      builtinLibs,
      env,
      prog!,
      out,
      out
    )
    machine.evaluate()
    expect(out.log).toEqual(expected)
}


describe('Basic codegen', () => {
  test('simple arithmetic', () => {
    checkMachineOutput(`
      (display (+ 1 1))  
    `, ['2'])
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
    `, ['120'])
  })

  test('basic list operations', () => {
    checkMachineOutput(`
      (define list-length
        (lambda (l)
          (if (null? l)
              0
              (+ 1 (list-length (cdr l))))))
      (display (list-length '()))
    `, ['0'])
  })

  test('basic struct operations', () => {
    checkMachineOutput(`
      (struct point (x y))
      (define p (point 1 2))
      (display (point-x p))
    `, ['1'])
  })

  test('nullary functions', () => {
    checkMachineOutput(`
      (define f (lambda () 1))
      (f)
    `, ['1'])
  })
})