import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// Regression for #453: `pixel-map` died with "Max call stack depth 10000
// exceeded!" on any image of more than about 10,000 pixels -- 100x100, and so
// every photograph a student brings to class.
//
// The chain is `pixel-map` -> `vector-map` -> `map` (all in src/lib), and
// `map` was written naively-recursive: one live frame per element, against a
// fiber limit of 10,000 (Fiber.maxCallStackDepth). The measured cliff is
// between 9,990 and 9,995 elements, so 100x100 = 10,000 is the smallest
// square image that reproduces it.
//
// The size is chosen to be the *cheapest* reproduction, not a realistic one:
// the interpreter costs roughly 0.4ms per element here, so 10,000 pixels
// already takes a few seconds and the 400x300 image from the issue would take
// the better part of a minute. Hence the explicit timeout.
describe('pixel-map over a large image (#453)', () => {
  test('a 100x100 image does not overrun the call stack', async () => {
    expect(
      await runProgram(`
(import image)
(import canvas)
(canvas? (pixel-map (lambda (p) p) (make-canvas 100 100)))
`),
    ).toEqual(['#t'])
  }, 60000)

  test('map over a 10,000-element list does not overrun the call stack', async () => {
    // The same failure, one layer down: this is what `pixel-map` bottoms out
    // in, and it is the definition that has to change.
    expect(await runProgram('(length (map (lambda (x) x) (range 10000)))')).toEqual([
      '10000',
    ])
  }, 60000)
})
