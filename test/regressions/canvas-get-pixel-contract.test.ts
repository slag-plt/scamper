import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/487
//
// `canvas-get-pixel`'s docstring declared `img : drawing?` for a parameter
// that is a canvas, and src/lib/index.ts turns that line into a runtime
// contract -- so the one kind of value the function works on was the one kind
// it rejected, with "expected a drawing, received object".
//
// These go through the Scheme binding on purpose. The existing coverage in
// test/libs/image.browser.test.ts calls `canvas_canvasGetPixel` directly,
// which never builds the contract wrapper and so cannot see this class of bug
// at all.
//
// jsdom's canvas mock does not round-trip pixel data (getImageData returns
// zeros, see the note on `pixel-map` in test/libs/image.test.ts), so the
// colour itself is checked against what `canvas->pixels` reports for the same
// pixel rather than against a literal -- an assertion that holds under both
// the mock and a real Canvas2D.
test('canvas-get-pixel accepts a canvas and returns that pixel', async () => {
  expect(await runProgram(`
  (import image)
  (define c (drawing->canvas (solid-square 10 "red")))
  (rgb? (canvas-get-pixel c 0 0))
  (equal? (canvas-get-pixel c 0 0) (vector-ref (canvas->pixels c) 0))
  `)).toEqual(['#t', '#t'])
})

// The parameter is still contracted, just as a canvas: a drawing -- the thing
// the old docstring named -- is not one.
test('canvas-get-pixel rejects a drawing via its contract', async () => {
  const out = await runProgram(`
  (import image)
  (canvas-get-pixel (solid-square 10 "red") 0 0)
  `)
  expect(out).toHaveLength(1)
  expect(out[0]).toContain('expected a canvas')
})
