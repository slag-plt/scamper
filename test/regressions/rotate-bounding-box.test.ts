import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/102
//
// `rotate` computed its post-rotation bounding box from a per-shape point set
// (`getDrawingPoints`) that disagreed with each drawing's declared width and
// height. Three bugs followed, all visible even at angle 0 (a no-op turn):
//
//   A. `ellipse` points were center-origin (+/-0.5w, +/-0.5h) while every
//      other shape was top-left origin, so a rotate-0 circle was shifted and
//      clipped and its width sampled as 99.90 instead of 100.
//   B. the `overlay` case called `drawings.reverse()`, mutating the shared
//      child array in place -- rotating an overlay flipped its z-order.
//   C. `path` reported only its content extent, ignoring the declared
//      width/height, so `(rotate 0 (path 200 200 ...))` shrank to its ink.
//
// The fix rotates the drawing's declared bounding-box corners instead, which
// is the identity at angle 0 for every shape and never touches the children.

describe('rotate bounding box (#102)', () => {
  async function dims (shape: string): Promise<string[]> {
    return runProgram(`
(import image)
(round (drawing-width ${shape}))
(round (drawing-height ${shape}))
`)
  }

  // Bug C: a rotate-0 path keeps the declared 200x200 box (was 20x10, the
  // extent of its four points), plus the line width its outline is drawn with
  // (#431).
  test('rotate 0 preserves a path\'s declared width/height', async () => {
    expect(await dims(`
      (rotate 0 (path 200 200
        (list (pair 90 80) (pair 110 80) (pair 110 70) (pair 90 70) (pair 90 80))
        "outline" "blue"))`)).toEqual(['201', '201'])
  })

  // Bug A: a rotate-0 circle is exactly its diameter, not the 99.90 that
  // perimeter sampling produced.
  test('rotate 0 gives a circle its exact 100x100 box', async () => {
    expect(await dims('(rotate 0 (solid-circle 100 "red"))')).toEqual(['100', '100'])
  })

  // Control: rectangles were always correct and must stay correct.
  test('rotate 0 leaves a rectangle unchanged', async () => {
    expect(await dims('(rotate 0 (solid-rectangle 100 40 "red"))')).toEqual(['100', '40'])
  })

  // Sanity: a real 90-degree turn still swaps width and height.
  test('rotate 90 swaps a rectangle\'s width and height', async () => {
    expect(await dims('(rotate 90 (solid-rectangle 100 40 "red"))')).toEqual(['40', '100'])
  })

  // Bug B: rotating an overlay must not mutate the input. The overlay renders
  // its children back-to-front, so the vector's child order is z-order; if
  // `rotate` reversed it in place the second print would show them swapped.
  test('rotate 0 does not reverse an overlay\'s children', async () => {
    const [before, , after] = await runProgram(`
(import image)
(define ov (overlay (solid-square 20 "red") (solid-square 20 "blue")))
ov
(rotate 0 ov)
ov
`)
    expect(after).toEqual(before)
    expect(before).toContain('255 0 0 255')
    // red must still precede blue in the child vector
    expect(before.indexOf('255 0 0 255')).toBeLessThan(before.indexOf('0 0 255 255'))
  })
})
