import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// Regression for #477: `set-maximum-recursion-depth!` built a tagged value,
// returned it, and nothing ever consumed it. The limit it claims to set is
// Fiber.maxCallStackDepth, so a student who hit "Max call stack depth 10000
// exceeded!", found the function named after their problem, and called it got
// a `[Blob: ...]` in their output and the same error on the next run.
//
// `sum` is deliberately *not* tail-recursive -- one live frame per level is
// the whole point -- and 12,000 levels is the cheapest depth that is clearly
// past the 10,000 default (about a second at the interpreter's ~0.06ms per
// frame).
const deepSum = (n: number) => `
(define sum
  (lambda (n)
    (if (= n 0)
        0
        (+ n (sum (- n 1))))))
(sum ${n.toString()})
`

describe('set-maximum-recursion-depth! (#477)', () => {
  test('raising the limit lets a deeper recursion finish', async () => {
    expect(
      await runProgram(`(set-maximum-recursion-depth! 20000)${deepSum(12000)}`),
    ).toEqual(['void', '72006000'])
  }, 30000)

  test('lowering the limit makes a shallower recursion fail', async () => {
    // The control: without it, a `set-maximum-recursion-depth!` that merely
    // raised the ceiling to something huge and ignored its argument would pass
    // the test above. The error also reports the limit it was given.
    expect(
      await runProgram(`(set-maximum-recursion-depth! 100)${deepSum(500)}`),
    ).toEqual(['void', 'Runtime error: Max call stack depth 100 exceeded!'])
  })
})
