import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/473
//
// `rotate` boxes a drawing by turning the corners of the box it is *given*, so
// a second turn rotates the first turn's padding as if it were ink: two
// 30-degree turns reported a 134.64 x 126.60 box where a single 60-degree turn
// reports 84.64 x 106.60, and `(rotate 45 (rotate -45 ...))` -- a no-op --
// doubled a 100x100 square to 200x200. The ink stays centred either way, so
// this is margin, not misplacement.

describe('nested rotate boxes like the single equivalent turn (#473)', () => {
  async function dims (shape: string): Promise<[number, number]> {
    const [width, height] = await runProgram(`
(import image)
(drawing-width ${shape})
(drawing-height ${shape})
`)
    return [Number(width), Number(height)]
  }

  // Exact equality, not `toBeCloseTo`: collapsing rebuilds the drawing as
  // `(rotate 60 d)` -- the same constructor on the same angle -- so the two
  // numbers are bit-identical, and anything less means the collapse did not
  // happen. Every nested-versus-single comparison below is exact for the same
  // reason.
  test('two 30-degree turns box like one 60-degree turn', async () => {
    const d = '(solid-rectangle 100 40 "red")'
    const [width, height] = await dims(`(rotate 60 ${d})`)
    const [nestedWidth, nestedHeight] = await dims(`(rotate 30 (rotate 30 ${d}))`)
    expect(nestedWidth).toBe(width)
    expect(nestedHeight).toBe(height)
  })

  test('a turn and its inverse leave a square\'s box alone', async () => {
    const [width, height] =
      await dims('(rotate 45 (rotate -45 (solid-square 100 "red")))')
    expect(width).toBe(100)
    expect(height).toBe(100)
  })

  // The summed angle is reduced modulo 360, which is what makes a whole turn
  // the exact identity: `(rotate 360 d)` on its own boxes 100 as
  // 100.00000000000001, since sin(2*pi) is not quite zero in floating point.
  // Asserted exactly on purpose -- toBeCloseTo would not see that drift.
  test('two half-turns leave the box exactly as it was', async () => {
    const [width, height] =
      await dims('(rotate 180 (rotate 180 (solid-rectangle 100 40 "red")))')
    expect(width).toBe(100)
    expect(height).toBe(40)
  })

  // JS's `%` keeps the dividend's sign, so a negative sum past a whole turn
  // reduces to a negative angle: -400 becomes -40, not 320. `rotate` takes a
  // negative angle happily and it names the same turn, so the box agrees --
  // closely rather than exactly, since -40 and 320 are different inputs to
  // sin/cos.
  test('a negative sum past a whole turn is still the same turn', async () => {
    const d = '(solid-rectangle 100 40 "red")'
    const [width, height] = await dims(`(rotate 320 ${d})`)
    const [nestedWidth, nestedHeight] = await dims(`(rotate -200 (rotate -200 ${d}))`)
    expect(nestedWidth).toBeCloseTo(width, 9)
    expect(nestedHeight).toBeCloseTo(height, 9)
  })

  // The collapse is bottom-up, so depth beyond two must fall out of the same
  // pass rather than needing a second one.
  test('three 20-degree turns box like one 60-degree turn', async () => {
    const d = '(solid-rectangle 100 40 "red")'
    const [width, height] = await dims(`(rotate 60 ${d})`)
    const [nestedWidth, nestedHeight] = await dims(`(rotate 20 (rotate 20 (rotate 20 ${d})))`)
    expect(nestedWidth).toBe(width)
    expect(nestedHeight).toBe(height)
  })

  // The padding does not stop at the rotate: beside/above/overlay snapshot
  // their children's boxes when they are built, so an inflated child inflated
  // its parent too. Fixing this is what the ancestor rebuild is for.
  test('the padding does not leak into a parent that measured the child', async () => {
    const nested = '(rotate 30 (rotate 30 (solid-rectangle 100 40 "red")))'
    const single = '(rotate 60 (solid-rectangle 100 40 "red"))'
    const [width, height] = await dims(`(beside ${single} (solid-square 10 "blue"))`)
    const [nestedWidth, nestedHeight] = await dims(`(beside ${nested} (solid-square 10 "blue"))`)
    expect(width).toBeCloseTo(94.6410161, 6)
    expect(nestedWidth).toBe(width)
    expect(nestedHeight).toBe(height)
  })

  // The boundary of the fix, pinned deliberately. A single turn still boxes the
  // corners of the box it is given, so a triangle keeps the slack between its
  // ink and that box. Tightening a shape to its ink is separate work that was
  // decided against here; a rotate over a non-rotate child must stay identical.
  test('a single turn over an unrotated shape is left exactly as it was', async () => {
    const [width, height] = await dims('(rotate 45 (solid-triangle 100 "red"))')
    expect(width).toBeCloseTo(131.9479216882342, 6)
    expect(height).toBeCloseTo(131.9479216882342, 6)
  })
})
