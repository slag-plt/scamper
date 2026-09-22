import { describe, expect, test } from 'vitest'
import { reductionTrace } from '../harness.js'

// Regression for #568: a traced `define` stopped one step short of its value.
// Only a `disp` step ever reached `stepper.final`, so a define's trace ended on
// the last reduction *inside* its expression -- `(define y (sqr (+ 2 3)))`
// finished at `(* 5 5)`, while the same expression on its own finished at `25`.
describe('a traced define ends at the value it binds (#568)', () => {
  test('a call reduces all the way to its result', async () => {
    expect(
      await reductionTrace(
        '(define sqr (lambda (x) (* x x)))\n(define y (sqr (+ 2 3)))',
      ),
    ).toEqual(['(sqr (+ 2 3))', '(sqr 5)', '(* 5 5)', '25'])
  })

  test('an arithmetic expression reduces to its number', async () => {
    expect(await reductionTrace('(define z (+ 1 2))')).toEqual([
      '(+ 1 2)',
      '3',
    ])
  })

  test('a define and the bare expression it wraps trace alike', async () => {
    const wrapped = await reductionTrace('(define y (+ 1 (* 2 3)))')
    expect(await reductionTrace('(+ 1 (* 2 3))')).toEqual(wrapped)
  })

  test('a define of a function does not repeat the closure as a step', async () => {
    // The exception: the step before already shows the lambda, so naming the
    // closure again would only add a `[Function: f]` line that says nothing.
    const trace = await reductionTrace('(define f (lambda (x) (* x 2)))')
    expect(trace.some((s) => s.includes('[Function'))).toBe(false)
  })
})
