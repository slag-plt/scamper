import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/253
//
// `rotate` used to build its bounding box from a per-shape point set whose
// `above` case referenced `xOffset` (a copy-paste slip from `beside`, the only
// place that name was declared), hitting the temporal dead zone so
// `(rotate <angle> (above ...))` threw `ReferenceError`. #102 later replaced
// that point set with the drawing's declared bounding-box corners, which never
// touches the `above` internals; these tests keep the guarantee that rotating
// an `above` succeeds and preserves its stacked dimensions.

describe('rotate of an above drawing (#253)', () => {
  // The crash: rotating an `above` produces a real drawing, not a runtime error.
  test('does not throw a ReferenceError', async () => {
    const result = await runProgram(`
(import image)
(rotate 45 (above (rectangle 20 20 "solid" "red") (rectangle 20 20 "solid" "blue")))
`)
    expect(result).toHaveLength(1)
    expect(result[0]).not.toContain('ReferenceError')
    expect(result[0]).toContain('rotate')
  })

  // The sibling `beside` case was never broken; keep it covered so a future
  // edit to the shared switch block can't regress it.
  test('the beside sibling still rotates cleanly', async () => {
    const result = await runProgram(`
(import image)
(> (drawing-width (rotate 45 (beside (rectangle 20 20 "solid" "red") (rectangle 20 20 "solid" "blue")))) 0)
`)
    expect(result).toEqual(['#t'])
  })

  // Geometry, not just "doesn't throw": stacking a 20-tall and a 30-tall box
  // (both 20 wide) is 20x50 unrotated, so a 90-degree turn swaps that to 50x20.
  test('a stacked above keeps its full height when rotated', async () => {
    expect(await runProgram(`
(import image)
(round (drawing-width (rotate 90 (above (rectangle 20 20 "solid" "red") (rectangle 20 30 "solid" "blue")))))
(round (drawing-height (rotate 90 (above (rectangle 20 20 "solid" "red") (rectangle 20 30 "solid" "blue")))))
`)).toEqual(['50', '20'])
  })

  // An `above` nested inside a `beside` inside a `rotate` exercises the branch
  // recursively.
  test('a nested above/beside combination rotates cleanly', async () => {
    expect(await runProgram(`
(import image)
(> (drawing-width (rotate 30 (beside (above (square 5 "solid" "red") (square 8 "solid" "blue")) (square 4 "solid" "green")))) 0)
`)).toEqual(['#t'])
  })
})
