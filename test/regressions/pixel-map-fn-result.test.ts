import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/553
//
// `pixel-map`'s contract constrains `fn` to a procedure but says nothing about
// what it *returns*, and the `pixels?` contract on `pixels->canvas` -- which
// would have caught it -- does not run, because `pixel-map` names
// `pixels->canvas` at top level from a library frame (see VarHandler, #476).
// `canvas_pixelsToCanvas` then read `.red` off a number, got undefined, and
// assigning undefined into a Uint8ClampedArray stores 0.
//
// So `(pixel-map (lambda (p) 5) c)` returned an all-black, fully transparent
// canvas with no error at all, while the same value handed to
// `pixels->canvas` directly was properly rejected.
//
// Fixed in `pixel-map` rather than by leaning on `pixels->canvas`'s own guard
// (which #553 also added): the mistake is in the `fn` the student wrote, and
// only `pixel-map` is in a position to say so.
describe('pixel-map checks what fn returns (#553)', () => {
  test('an fn that does not return an rgb value is an error, naming fn', async () => {
    expect(
      await runProgram(`
(import image)
(import canvas)
(pixel-map (lambda (p) 5) (make-canvas 2 2))
`),
    ).toEqual([
      'Runtime error [3:1-3:44]: (error) pixel-map: expected fn to return an rgb value for each pixel',
    ])
  })

  test('the same value reaches pixels->canvas with a contract of its own', async () => {
    // The asymmetry that made the bug: this call was always rejected.
    expect(
      await runProgram(`
(import image)
(pixels->canvas (vector 5 5 5 5) 2 2)
`),
    ).toEqual([
      'Runtime error [2:1-2:37]: (error) expected a pixels as the first argument, received vector',
    ])
  })

  test('an fn that does return rgb values still maps the canvas', async () => {
    expect(
      await runProgram(`
(import image)
(import canvas)
(canvas? (pixel-map (lambda (p) (rgb 0 0 0)) (make-canvas 2 2)))
`),
    ).toEqual(['#t'])
  })
})
