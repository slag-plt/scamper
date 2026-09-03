// Runs under real headless Chromium (see test/vitest.browser.config.ts), so
// this asserts what a student actually sees rather than what the struct says.
//
// https://github.com/slag-plt/scamper/issues/454: `rotate`'s docstring warned
// that it "shifts off-center" long after the geometry was fixed. Its sibling
// rotate-centering.test.ts checks the box `drawing_rotate` computes; this one
// checks the pixels `drawing_render` paints, so the rendering half of the turn
// cannot drift off-centre while the struct still looks right.
import { expect, test } from 'vitest'
import {
  drawing_drawingToCanvas,
  drawing_rectangle,
  drawing_rotate,
} from '../../src/js/image/drawing.js'

// A 100x100 square turned 45 degrees is a diamond in a 141.42 box, rounded up
// to a 142x142 canvas, with a vertex at the middle of each edge.
const SIZE = 142
const MIDDLE = 70.7

// How far in from an edge to probe. The outermost pixel of a tip is only
// partly covered, and the box is a rounded-up 142 against a drawing 141.42
// wide, so the last row and column are fractional.
const NEAR = 2
const FAR = SIZE - 1 - NEAR

/** Every pixel of `canvas`, read once: a getImageData per probe is slow. */
function pixels(canvas: HTMLCanvasElement): ImageData {
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new Error('no canvas context')
  }
  return ctx.getImageData(0, 0, canvas.width, canvas.height)
}

/** True if anything was drawn over the white background at this pixel. */
function inked(image: ImageData, x: number, y: number): boolean {
  const i = (y * image.width + x) * 4
  return image.data[i + 1] < 255 || image.data[i + 2] < 255
}

/**
 * The first and last inked position along one row or column of `image`.
 * @returns the pair, or undefined if nothing was drawn in it.
 */
function run(
  image: ImageData, index: number, axis: 'row' | 'column',
): [number, number] | undefined {
  const length = axis === 'row' ? image.width : image.height
  const hits = [...Array(length).keys()].filter((i) =>
    axis === 'row' ? inked(image, i, index) : inked(image, index, i))
  return hits.length === 0 ? undefined : [hits[0], hits[hits.length - 1]]
}

/**
 * Asserts that the ink along one row or column is centred on the box. The
 * tolerance absorbs anti-aliasing at the diamond's shallow tips, where a
 * partly covered pixel may or may not read as inked.
 */
function expectCentred(image: ImageData, index: number, axis: 'row' | 'column') {
  const span = run(image, index, axis)
  expect(span, `${axis} ${index} has no ink`).toBeDefined()
  const [first, last] = span ?? [0, 0]
  const middle = (first + last) / 2
  expect(middle, `${axis} ${index} spans ${first}..${last}`)
    .toBeGreaterThanOrEqual(MIDDLE - 1)
  expect(middle, `${axis} ${index} spans ${first}..${last}`)
    .toBeLessThanOrEqual(MIDDLE + 1)
}

/** A red square turned 45 degrees, rendered as a student would see it. */
function diamond(): HTMLCanvasElement {
  return drawing_drawingToCanvas(
    drawing_rotate(45, drawing_rectangle(100, 100, 'solid', 'red')))
}

test('a square rotated 45 degrees is drawn as a diamond, not a shifted square (#454)', () => {
  const canvas = diamond()
  expect([canvas.width, canvas.height]).toEqual([SIZE, SIZE])
  const image = pixels(canvas)

  // The middle is filled and all four corners of the box are bare: a square
  // that had merely been shifted would have covered at least one of them.
  expect(inked(image, SIZE / 2, SIZE / 2)).toBe(true)
  for (const [x, y] of [[NEAR, NEAR], [FAR, NEAR], [NEAR, FAR], [FAR, FAR]]) {
    expect(inked(image, x, y), `corner (${x}, ${y}) should be bare`).toBe(false)
  }
})

test('a vertex of the diamond reaches the middle of every edge (#454)', () => {
  const image = pixels(diamond())
  const mid = Math.round(MIDDLE)
  for (const [x, y] of [[NEAR, mid], [FAR, mid], [mid, NEAR], [mid, FAR]]) {
    expect(inked(image, x, y), `edge midpoint (${x}, ${y}) should be inked`).toBe(true)
  }
})

test('the diamond is centred in its box, edge to edge (#454)', () => {
  const image = pixels(diamond())

  // Each tip is centred across the edge it touches, and the widest row and
  // column are centred too -- the invariant the docstring used to doubt.
  for (const axis of ['row', 'column'] as const) {
    expectCentred(image, NEAR, axis)
    expectCentred(image, Math.round(MIDDLE), axis)
    expectCentred(image, FAR, axis)
  }
})
