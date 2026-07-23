import { expect, test, describe } from 'vitest'

import * as S from '../../src/scheme'
import * as L from '../../src/lpm'
import { Fiber } from '../../src/lpm/fiber'

async function checkMachineOutput (src: string, expected: L.Value[]) {
  const out = new L.LoggingChannel(false, false)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const prog = (await S.compile(out, src))!
  expect(out.errLog).toEqual([])
  const fiber = new Fiber(prog, S.mkInitialEnv())
  // TODO: this should be refactored once we've re-established a common
  // entry point for running Scamper programs
  while (!fiber.isDone()) {
    const res = fiber.step()
    if (res.tag === 'display') {
      out.send(fiber.lastResult)
    }
  }
  expect(out.log).toEqual(expected)
}

describe('Basic codegen', () => {
  test('simple arithmetic', async () => {
    await checkMachineOutput(`
      (display (+ 1 1))
    `, [2])
  })
})

describe('End-to-end cases', () => {
  test('factorial', async () => {
    await checkMachineOutput(`
      (define fact
        (lambda (n)
          (if (zero? n)
              1
              (* n (fact (- n 1))))))

      (display (fact 5))
    `, [120])
  })

  test('basic list operations', async () => {
    await checkMachineOutput(`
      (define list-length
        (lambda (l)
          (if (null? l)
              0
              (+ 1 (list-length (cdr l))))))
      (display (list-length '()))
    `, [0])
  })

  test('basic struct operations', async () => {
    await checkMachineOutput(`
      (struct point (x y))
      (define p (point 1 2))
      (display (point-x p))
    `, [1])
  })

  test('nullary functions', async () => {
    await checkMachineOutput(`
      (define f (lambda () 1))
      (f)
    `, [1])
  })
})