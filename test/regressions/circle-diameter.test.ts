import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/433
//
// `circle` and its two fill-mode shorthands took a radius, so the one number
// they take meant something different from the one `square` takes:
// (solid-circle 100 "red") was twice the size of (solid-square 100 "red").
// They take a diameter now, so every shape constructor's size argument is the
// size of the shape.

test('a circle is as wide as a square of the same size (#433)', async () => {
  expect(
    await runProgram(`
  (import image)
  (drawing-width (solid-circle 100 "red"))
  (drawing-height (solid-circle 100 "red"))
  (drawing-width (solid-square 100 "red"))
  (equal? (solid-circle 100 "red") (solid-ellipse 100 100 "red"))
  `),
  ).toEqual(['100', '100', '100', '#t'])
})

test('every circle constructor takes a diameter (#433)', async () => {
  expect(
    await runProgram(`
  (import image)
  (circle 10 "solid" "red")
  (solid-circle 10 "red")
  (outlined-circle 10 "red" 1)
  `),
  ).toEqual([
    '(ellipse 10 10 "solid" (rgba 255 0 0 255))',
    '(ellipse 10 10 "solid" (rgba 255 0 0 255))',
    '(ellipse 11 11 "outline" (rgba 255 0 0 255) 1)', // plus the line width (#431)
  ])
})
