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
