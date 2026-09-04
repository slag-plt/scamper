import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/491
//
// `with-dash` documents its `dash-spec` as a `list?`, but its native stored
// that list in the drawing struct as-is and the renderer handed it straight to
// `ctx.setLineDash`, which takes a Javascript array -- so every call from
// Scheme died at *render* time, an empty list included. The existing library
// tests only asserted the struct `with-dash` builds, never drew it, which is
// why the whole function was broken and green.

test('a dashed drawing renders instead of failing in setLineDash (#491)', async () => {
  expect(
    await runProgram(`
(import image)
(vector-length (drawing->pixels (with-dash (list 4 2) (outlined-square 20 "red"))))
(vector-length (drawing->pixels (with-dash (list) (outlined-square 20 "red"))))
`),
  ).toEqual(['441', '441'])
})

// The conversion checks its elements because `setLineDash` does not: per spec
// it silently returns on a spec it cannot read, so an unchecked one would draw
// a plain solid line and say nothing. A negative or non-finite length hits that
// same rule as squarely as a string does -- measured in Chromium, an outlined
// square strokes 80 red pixels solid and 30 with (list 4 4), but 80 again with
// (list 4 -4).
test('a dash spec setLineDash would silently ignore is an error, not a solid line (#491)', async () => {
  expect(
    await runProgram(`
(import image)
(with-dash (list 4 "two") (outlined-square 20 "red"))
(with-dash (list 4 -2) (outlined-square 20 "red"))
(with-dash (list 4 (sqrt -1)) (outlined-square 20 "red"))
`, { stripRanges: true }),
  ).toEqual([
    'Runtime error: (with-dash) expected a list of numbers, but the list contains string',
    'Runtime error: (with-dash) expected a finite, non-negative dash length, received -2',
    'Runtime error: (with-dash) expected a finite, non-negative dash length, received NaN',
  ])
})
