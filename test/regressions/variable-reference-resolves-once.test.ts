import { describe, expect, test, vi } from 'vitest'
import * as S from '../../src/scheme/index.js'
import { Fiber } from '../../src/lpm/fiber.js'
import { diagnosticToError } from '../../src/scheme/diagnostic'
import { Env } from '../../src/lpm/lang.js'
import * as L from '../../src/lpm/index.js'
import { stepFiberWith } from '../util.js'

// https://github.com/slag-plt/scamper/issues/730
//
// `pixel-map` was slow enough to be unusable on a photo-sized canvas. The cause
// was not the algorithm -- `vector-map` already fills a result vector directly
// (#453) -- but the cost of a single variable reference, the most common op a
// program runs.
//
// `VarHandler` resolved each name *three* times: `has`, then `get`, then
// `isLocal`. Each one walked every local scope, the top level, and then every
// imported library, and step 3 of `Env.lookup` rebuilt the import list (two
// array allocations) on each walk. A name from the standard library -- which is
// most names, `vector-ref` and `vector-set!` included -- reached that step every
// time.
//
// As in tco-frame-depth (#316), the property is asserted by *counting* rather
// than by timing: a wall-clock budget goes flaky under parallel load, while the
// number of environment walks per variable reference is exact and catches a
// regression on the first reference rather than once a program is visibly slow.

/** Runs `prog` to completion, counting how often the environment is walked. */
function countLookups(prog: L.Prog): { lookups: number; varOps: number } {
  const spy = vi.spyOn(Env.prototype, 'lookup')
  let varOps = 0
  try {
    const fiber = new Fiber(prog, S.mkInitialEnv())
    stepFiberWith(fiber, (f) => {
      // The op *about* to run, counted before the step pops it. `Frame.ops` is
      // a stack, so the next one is its last element.
      const op = f.frames.at(-1)?.ops.at(-1)
      if (op?.tag === 'var') varOps++
    })
    return { lookups: spy.mock.calls.length, varOps }
  } finally {
    spy.mockRestore()
  }
}

async function compileOrThrow(src: string): Promise<L.Prog> {
  const { prog, diagnostics } = await S.compile(src.trim())
  const errors = diagnostics.map((d) => diagnosticToError(d).toString())
  expect(errors).toEqual([])
  if (prog === undefined) throw new Error('compile produced no program')
  return prog
}

describe('a variable reference resolves its name once (#730)', () => {
  // A loop over a vector through the standard library: every iteration names
  // `vector-ref`, `vector-set!`, and `for-range`, none of which are local, so
  // each reference runs the full walk the bug tripled.
  const vectorMap = `
(define v (make-vector 40 1))
(define r (vector-map (lambda (x) (+ x 1)) v))
(vector-ref r 0)
`

  test('one environment walk per variable reference', async () => {
    const { lookups, varOps } = countLookups(await compileOrThrow(vectorMap))
    // A `var` op is the only thing that resolves a name while a program runs,
    // and it now does so exactly once. Anything above `varOps` means a handler
    // has gone back to the environment for a name it had already resolved --
    // the regression this pins. (Below is impossible: every `var` resolves.)
    expect(varOps).toBeGreaterThan(100)
    expect(lookups).toBe(varOps)
  })

  test('the vector is still mapped correctly', async () => {
    const prog = await compileOrThrow(vectorMap)
    const fiber = new Fiber(prog, S.mkInitialEnv())
    stepFiberWith(fiber)
    expect(fiber.lastResult).toBe(2)
  })
})
