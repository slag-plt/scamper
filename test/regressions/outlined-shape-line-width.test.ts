import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/431
//
// `outlined-circle` took no line width, and the stroke it did draw straddled
// the shape's own bounding box, so half of it fell outside and was cut off.
// It now takes one, as Racket's does, and an outlined shape's box is its size
// plus its line width -- a circle of diameter 30 stroked 10 wide needs 40x40
// to hold it -- so nothing is truncated.

test('outlined-circle takes a line width, and its box holds the whole stroke (#431)', async () => {
  expect(
    await runProgram(`
  (import image)
  (outlined-circle 30 "red" 10)
  (drawing-width (outlined-circle 30 "red" 10))
  (drawing-height (outlined-circle 30 "red" 10))
  (drawing-width (outlined-circle 30 "red" 1))
  `),
  ).toEqual([
    '(ellipse 40 40 "outline" (rgba 255 0 0 255) 10)',
    '40',
    '40',
    '31',
  ])
})

test('a solid shape is unchanged and carries no line width (#431)', async () => {
  expect(
    await runProgram(`
  (import image)
  (solid-circle 30 "red")
  (drawing-width (solid-circle 30 "red"))
  (drawing-width (solid-square 100 "blue"))
  `),
  ).toEqual([
    '(ellipse 30 30 "solid" (rgba 255 0 0 255))',
    '30',
    '100',
  ])
})

test('circle takes an optional line width, ignored when solid (#431)', async () => {
  expect(
    await runProgram(`
  (import image)
  (circle 30 "outline" "red" 6)
  (circle 30 "outline" "red")
  (circle 30 "solid" "red")
  `),
  ).toEqual([
    '(ellipse 36 36 "outline" (rgba 255 0 0 255) 6)',
    '(ellipse 31 31 "outline" (rgba 255 0 0 255) 1)',
    '(ellipse 30 30 "solid" (rgba 255 0 0 255))',
  ])
})

test('every other outlined shape leaves room for its stroke too (#431)', async () => {
  expect(
    await runProgram(`
  (import image)
  (drawing-width (outlined-square 100 "red"))
  (drawing-width (outlined-rectangle 100 50 "red"))
  (drawing-width (outlined-triangle 100 "red"))
  `),
  ).toEqual(['101', '101', '101'])
})

test('a line width must be positive (#431)', async () => {
  expect(
    await runProgram(
      `
  (import image)
  (outlined-circle 30 "red" 0)
  (outlined-circle 30 "red" -5)
  `,
      { stripRanges: true },
    ),
  ).toEqual([
    'Runtime error: (outlined-circle) expected a positive line width, received 0',
    'Runtime error: (outlined-circle) expected a positive line width, received -5',
  ])
})

test('recoloring an outlined shape keeps its line width and its box (#431)', async () => {
  expect(
    await runProgram(`
  (import image)
  (drawing-recolor (outlined-circle 30 "red" 10) "blue")
  (drawing-recolor (drawing-recolor (outlined-circle 30 "red" 10) "blue") "green")
  (drawing-recolor (solid-circle 30 "red") "blue")
  `),
  ).toEqual([
    '(ellipse 40 40 "outline" (rgba 0 0 255 255) 10)',
    '(ellipse 40 40 "outline" (rgba 0 128 0 255) 10)',
    '(ellipse 30 30 "solid" (rgba 0 0 255 255))',
  ])
})
