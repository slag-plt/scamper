import { describe, expect, test } from 'vitest'
import * as S from '../../src/scheme/index.js'
import { Fiber } from '../../src/lpm/fiber.js'
import { diagnosticToError } from '../../src/scheme/diagnostic'
import * as L from '../../src/lpm/index.js'
import { stepFiberWith } from '../util.js'

// https://github.com/slag-plt/scamper/issues/316
//
// Tail-call optimization was asserted *indirectly*: run 12000 iterations and
// rely on the fiber's 10000-frame limit to error if frames accumulated. That
// cost ~9.2 million interpreter steps (~4s on an idle machine, 77% of vitest's
// 5s default), so it went flaky under parallel load and needed a 30s timeout to
// keep `validate` green -- and it only noticed a leak once 10000 frames had
// piled up.
//
// The property is observable directly: a tail call replaces the current frame
// (see the canTailCall branch in op-handlers.ts), so a tail-recursive loop runs
// at a *constant* frame depth no matter how many iterations it makes. The
// measured peak for the loop below is 6. Watching fiber.frames.length says
// exactly that, catches a leak on the first iteration, and costs milliseconds.

/** Runs `prog` to completion, returning its peak frame depth and last value. */
function runTracked(prog: L.Prog): { maxFrames: number; result: L.Value } {
  const fiber = new Fiber(prog, S.mkInitialEnv())
  let maxFrames = 0
  stepFiberWith(fiber, (f) => {
    maxFrames = Math.max(maxFrames, f.frames.length)
  })
  return { maxFrames, result: fiber.lastResult }
}

async function compileOrThrow(src: string): Promise<L.Prog> {
  const { prog, diagnostics } = await S.compile(src.trim())
  const errors = diagnostics.map((d) => diagnosticToError(d).toString())
  expect(errors).toEqual([])
  if (prog === undefined) throw new Error('compile produced no program')
  return prog
}

/** A tail call in the else-branch of an `if`, inside a `let` body. */
const countTo = (n: number) => `
(define count
  (lambda (n acc)
    (if (= n 0)
        acc
        (let ([m (- n 1)])
          (count m (+ acc 1))))))
(count ${n.toString()} 0)
`

describe('tail calls run at a constant frame depth', () => {
  test('frame depth does not grow with the number of iterations', async () => {
    const shallow = runTracked(await compileOrThrow(countTo(10)))
    const deeper = runTracked(await compileOrThrow(countTo(400)))

    // The answer is still right...
    expect(shallow.result).toBe(10)
    expect(deeper.result).toBe(400)
    // ...and 40x the iterations costs exactly zero extra frames. Without TCO
    // this grows by one frame per iteration.
    expect(deeper.maxFrames).toBe(shallow.maxFrames)
  })

  test('the peak frame depth is a small constant', async () => {
    // Pinned so a regression that merely *slows* frame growth (rather than
    // stopping it) still fails, instead of quietly passing under the 10000
    // limit. The observed peak is 6; the bound leaves room for the evaluator
    // to change shape without being meaninglessly loose.
    const { maxFrames } = runTracked(await compileOrThrow(countTo(400)))
    expect(maxFrames).toBeLessThanOrEqual(16)
  })

  test('a non-tail-recursive call still does grow the stack', async () => {
    // The control: `(+ 1 (sum ...))` leaves work after the call, so it is not a
    // tail call and frames must accumulate. This is what keeps the two tests
    // above honest -- if frame depth were constant for *every* program, they
    // would pass no matter what TCO did.
    const src = `
(define sum
  (lambda (n)
    (if (= n 0)
        0
        (+ n (sum (- n 1))))))
(sum 100)
`
    const { maxFrames, result } = runTracked(await compileOrThrow(src))
    expect(result).toBe(5050)
    expect(maxFrames).toBeGreaterThan(100)
  })
})
