// Runs under real headless Chromium (see test/vitest.browser.config.ts), so
// these assert what a student actually sees rather than what the struct says.
//
// https://github.com/slag-plt/scamper/issues/431: a wide outline used to be
// cut off, because the stroke straddled a box exactly the shape's own size.
import { expect, test } from 'vitest'
import {
  drawing_circle,
  drawing_drawingToCanvas,
  drawing_rectangle,
} from '../../src/js/image/drawing.js'

function pixel(canvas: HTMLCanvasElement, x: number, y: number): number[] {
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new Error('no canvas context')
  }
  return Array.from(ctx.getImageData(x, y, 1, 1).data)
}

const red = [255, 0, 0, 255]
const white = [255, 255, 255, 255]

/** True if anything was drawn over the white background at this pixel. */
function inked(canvas: HTMLCanvasElement, x: number, y: number): boolean {
  const [, g, b] = pixel(canvas, x, y)
  return g < 255 || b < 255
}

test('a wide circle outline reaches its box and is not cut off (#431)', () => {
  // Diameter 30 stroked 10 wide: the ring runs from 25 across to 35 across, so
  // the box is 40x40 and the stroke touches every edge of it.
  const canvas = drawing_drawingToCanvas(drawing_circle(30, 'outline', 'red', 10))
  expect(canvas.width).toBe(40)
  expect(canvas.height).toBe(40)
  // The outermost pixel of a curve is only partly covered, so it is checked
  // for ink rather than for exactly red; a pixel in from it is fully red.
  expect(inked(canvas, 0, 20)).toBe(true) // left edge, mid-height
  expect(inked(canvas, 39, 20)).toBe(true) // right edge
  expect(inked(canvas, 20, 0)).toBe(true) // top edge
  expect(inked(canvas, 20, 39)).toBe(true) // bottom edge
  expect(pixel(canvas, 1, 20)).toEqual(red)
  expect(pixel(canvas, 20, 20)).toEqual(white) // the hole in the middle
})

test('a wide rectangle outline keeps its square corners (#431)', () => {
  // A right angle's miter reaches exactly the corner of the box, so the corner
  // pixel is inked; a round join would have left it blank.
  const canvas = drawing_drawingToCanvas(drawing_rectangle(40, 40, 'outline', 'red', 10))
  expect(canvas.width).toBe(50)
  expect(canvas.height).toBe(50)
  expect(pixel(canvas, 0, 0)).toEqual(red)
  expect(pixel(canvas, 49, 49)).toEqual(red)
  expect(pixel(canvas, 25, 25)).toEqual(white)
})
