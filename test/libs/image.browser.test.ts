// Runs under real headless Chromium (see test/vitest.browser.config.ts), not
// jsdom, so pixel and font-metric assertions reflect actual Canvas2D
// behavior rather than a mock. Two real-browser wrinkles worth knowing, both
// artifacts of internal premultiplied-alpha storage: (1) putImageData then
// getImageData loses RGB entirely for a fully transparent pixel -- it always
// reads back as (0, 0, 0, 0); (2) for a partially transparent pixel, a
// mid-range color channel can round off by one. So the known pixel arrays
// below only use non-zero alpha, and only use mid-range color channels
// alongside fully opaque (alpha 255) pixels, where premultiplication is exact.
import { describe, expect, test } from 'vitest'
import * as L from '../../src/lpm'
import {
  canvas_canvasGetPixel,
  canvas_canvasSetPixels,
  canvas_canvasToPixels,
  canvas_pixelsToCanvas,
} from '../../src/js/canvas/index.js'
import {
  color_colorToRgb,
  color_hsv,
  color_hsvAlpha,
  color_hsvComplement,
  color_hsvHue,
  color_hsvSaturation,
  color_hsvToRgb,
  color_hsvToString,
  color_hsvValue,
  color_rgb,
  color_rgbToHsv,
} from '../../src/js/image/color.js'
import {
  drawing_above,
  drawing_beside,
  drawing_drawingToCanvas,
  drawing_drawingToPixels,
  drawing_ellipse,
  drawing_isoscelesTriangle,
  drawing_overlay,
  drawing_overlayOffset,
  drawing_path,
  drawing_rectangle,
  drawing_rotate,
  drawing_text,
  drawing_withDash,
} from '../../src/js/image/drawing.js'

function makeCanvas(width: number, height: number): HTMLCanvasElement {
  const canvas = document.createElement('canvas')
  canvas.width = width
  canvas.height = height
  return canvas
}

function throwError(msg: string): never {
  throw new Error(msg)
}

function context2d(canvas: HTMLCanvasElement): CanvasRenderingContext2D {
  return canvas.getContext('2d') ?? throwError('no canvas context')
}

function pixel(canvas: HTMLCanvasElement, x: number, y: number): number[] {
  return Array.from(context2d(canvas).getImageData(x, y, 1, 1).data)
}

// True if any pixel departs from the opaque-white background drawing_clearDrawing
// lays down -- i.e. the drawing actually put ink on the canvas.
function hasColoredPixel(canvas: HTMLCanvasElement): boolean {
  const data = context2d(canvas).getImageData(0, 0, canvas.width, canvas.height).data
  for (let i = 0; i < data.length; i += 4) {
    if (data[i] !== 255 || data[i + 1] !== 255 || data[i + 2] !== 255) {
      return true
    }
  }
  return false
}

// Count of fully-red, fully-opaque pixels -- a size-independent measure of how
// much of a red shape actually landed on the canvas.
function redPixelCount(canvas: HTMLCanvasElement): number {
  const data = context2d(canvas).getImageData(0, 0, canvas.width, canvas.height).data
  let count = 0
  for (let i = 0; i < data.length; i += 4) {
    if (data[i] === 255 && data[i + 1] === 0 && data[i + 2] === 0 && data[i + 3] === 255) {
      count++
    }
  }
  return count
}

describe('text', () => {
  test('reports positive width and height', () => {
    const t = drawing_text('hello', 20, color_rgb(0, 0, 0, 255))
    expect(t.width).toBeGreaterThan(0)
    expect(t.height).toBeGreaterThan(0)
  })

  test('longer text is wider', () => {
    const short = drawing_text('a', 20, color_rgb(0, 0, 0, 255))
    const long = drawing_text('a much longer piece of text', 20, color_rgb(0, 0, 0, 255))
    expect(long.width).toBeGreaterThan(short.width)
  })

  test('a larger size produces a larger width and height', () => {
    const small = drawing_text('hello', 10, color_rgb(0, 0, 0, 255))
    const big = drawing_text('hello', 40, color_rgb(0, 0, 0, 255))
    expect(big.width).toBeGreaterThan(small.width)
    expect(big.height).toBeGreaterThan(small.height)
  })

  test('empty text has zero width', () => {
    const t = drawing_text('', 20, color_rgb(0, 0, 0, 255))
    expect(t.width).toBe(0)
  })
})

describe('drawing->image', () => {
  test('renders a single shape to a canvas of the drawing size', () => {
    const canvas = drawing_drawingToCanvas(drawing_rectangle(2, 2, 'solid', 'red'))
    expect(canvas.width).toBe(2)
    expect(canvas.height).toBe(2)
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 1, 1)).toEqual([255, 0, 0, 255])
  })

  test('renders a composite drawing tree, positioning each subdrawing', () => {
    const tree = drawing_beside(
      drawing_rectangle(2, 2, 'solid', 'red'),
      drawing_rectangle(2, 2, 'solid', 'blue'),
    )
    const canvas = drawing_drawingToCanvas(tree)
    expect(canvas.width).toBe(4)
    expect(canvas.height).toBe(2)
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 3, 1)).toEqual([0, 0, 255, 255])
  })
})

// One test per drawing_render branch, driven end-to-end through a real Canvas2D.
// rectangle and beside are already exercised by drawing->image above.
describe('render per-shape branches', () => {
  test('ellipse fills its interior', () => {
    const canvas = drawing_drawingToCanvas(drawing_ellipse(10, 10, 'solid', 'red'))
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(10)
    expect(pixel(canvas, 5, 5)).toEqual([255, 0, 0, 255])
  })

  test('triangle fills its interior', () => {
    const canvas = drawing_drawingToCanvas(drawing_isoscelesTriangle(10, 10, 'solid', 'blue'))
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(10)
    expect(pixel(canvas, 5, 8)).toEqual([0, 0, 255, 255])
  })

  test('path fills the polygon it traces', () => {
    const points = L.mkList(
      L.mkPair(0, 0), L.mkPair(10, 0), L.mkPair(10, 10), L.mkPair(0, 10),
    )
    const canvas = drawing_drawingToCanvas(drawing_path(10, 10, points, 'solid', 'green'))
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(10)
    // green is (0, 128, 0); alpha 255 keeps the mid-range channel exact
    expect(pixel(canvas, 5, 5)).toEqual([0, 128, 0, 255])
  })

  test('ellipse outline strokes its boundary', () => {
    const canvas = drawing_drawingToCanvas(drawing_ellipse(10, 10, 'outline', 'red'))
    // An outlined shape's box is its size plus its line width (#431).
    expect(canvas.width).toBe(11)
    expect(canvas.height).toBe(11)
    expect(hasColoredPixel(canvas)).toBe(true)
  })

  test('triangle outline strokes its edges', () => {
    const canvas = drawing_drawingToCanvas(drawing_isoscelesTriangle(10, 10, 'outline', 'blue'))
    expect(canvas.width).toBe(11)
    expect(canvas.height).toBe(11)
    expect(hasColoredPixel(canvas)).toBe(true)
  })

  test('path outline strokes the polyline it traces', () => {
    const points = L.mkList(L.mkPair(1, 1), L.mkPair(8, 1), L.mkPair(4, 8))
    const canvas = drawing_drawingToCanvas(drawing_path(10, 10, points, 'outline', 'green'))
    expect(canvas.width).toBe(11)
    expect(canvas.height).toBe(11)
    expect(hasColoredPixel(canvas)).toBe(true)
  })

  test('above stacks subdrawings vertically', () => {
    const canvas = drawing_drawingToCanvas(drawing_above(
      drawing_rectangle(4, 4, 'solid', 'red'),
      drawing_rectangle(4, 4, 'solid', 'blue'),
    ))
    expect(canvas.width).toBe(4)
    expect(canvas.height).toBe(8)
    expect(pixel(canvas, 2, 1)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 2, 6)).toEqual([0, 0, 255, 255])
  })

  test('overlay draws the first drawing on top of the rest', () => {
    const canvas = drawing_drawingToCanvas(drawing_overlay(
      drawing_rectangle(4, 4, 'solid', 'blue'),
      drawing_rectangle(8, 8, 'solid', 'red'),
    ))
    expect(canvas.width).toBe(8)
    expect(canvas.height).toBe(8)
    expect(pixel(canvas, 4, 4)).toEqual([0, 0, 255, 255])
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
  })

  test('overlay/offset shifts the second drawing and keeps the first on top', () => {
    const canvas = drawing_drawingToCanvas(drawing_overlayOffset(
      2, 2,
      drawing_rectangle(4, 4, 'solid', 'red'),
      drawing_rectangle(4, 4, 'solid', 'blue'),
    ))
    expect(canvas.width).toBe(6)
    expect(canvas.height).toBe(6)
    expect(pixel(canvas, 1, 1)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 5, 5)).toEqual([0, 0, 255, 255])
  })

  test('rotate fills the interior of its grown bounding box', () => {
    const canvas = drawing_drawingToCanvas(drawing_rotate(90, drawing_rectangle(10, 20, 'solid', 'red')))
    expect(canvas.width).toBeGreaterThan(0)
    expect(canvas.height).toBeGreaterThan(0)
    expect(pixel(canvas, 10, 5)).toEqual([255, 0, 0, 255])
  })

  test('with-dash strokes a dashed outline', () => {
    const canvas = drawing_drawingToCanvas(drawing_withDash([4, 4], drawing_rectangle(20, 20, 'outline', 'red')))
    expect(canvas.width).toBe(21)
    expect(canvas.height).toBe(21)
    expect(hasColoredPixel(canvas)).toBe(true)
  })

  test('text draws visible ink on the canvas', () => {
    const canvas = drawing_drawingToCanvas(drawing_text('H', 40, color_rgb(0, 0, 0, 255)))
    expect(canvas.width).toBeGreaterThan(0)
    expect(canvas.height).toBeGreaterThan(0)
    expect(hasColoredPixel(canvas)).toBe(true)
  })
})

// https://github.com/slag-plt/scamper/issues/102
//
// A rotate-0 turn is a no-op and must render identically to the un-rotated
// drawing. The old bounding box came from a per-shape point set that disagreed
// with the declared width/height, so `rotate` shifted, clipped, and reordered
// even at angle 0. These pixel checks are the real proof the fix landed; the
// jsdom suite (test/regressions/rotate-bounding-box.test.ts) covers dimensions.
describe('rotate 0 is a no-op (#102)', () => {
  // Bug A: an ellipse's points were center-origin while every other shape was
  // top-left origin, so rotate-0 translated the circle down-right by its radius
  // -- its center read as background and ~3/4 of its ink fell off the canvas.
  test('does not shift or clip a circle', () => {
    const circle = drawing_ellipse(20, 20, 'solid', 'red')
    const plain = drawing_drawingToCanvas(circle)
    const rotated = drawing_drawingToCanvas(drawing_rotate(0, circle))
    // center pixel is red, not the white background it clipped to before
    expect(pixel(rotated, 10, 10)).toEqual([255, 0, 0, 255])
    // and about as much red survives as the un-rotated circle, not ~1/4
    const plainRed = redPixelCount(plain)
    expect(redPixelCount(rotated)).toBeGreaterThan(plainRed * 0.9)
  })

  // Bug B: the overlay case reversed the shared child array in place, so
  // rotating an overlay flipped its z-order -- and mutated the caller's input.
  test('does not reverse an overlay it wraps', () => {
    const ov = drawing_overlay(
      drawing_rectangle(20, 20, 'solid', 'red'),
      drawing_rectangle(20, 20, 'solid', 'blue'),
    )
    // red is listed first, so it draws on top: the center is red
    expect(pixel(drawing_drawingToCanvas(ov), 10, 10)).toEqual([255, 0, 0, 255])
    // rotating the overlay must not touch `ov`...
    drawing_rotate(0, ov)
    // ...so re-rendering the SAME overlay still shows red on top
    expect(pixel(drawing_drawingToCanvas(ov), 10, 10)).toEqual([255, 0, 0, 255])
  })
})

describe('drawing->pixels', () => {
  test('flattens a rendered drawing into a row-major Rgb array', () => {
    const tree = drawing_beside(
      drawing_rectangle(2, 2, 'solid', 'red'),
      drawing_rectangle(2, 2, 'solid', 'blue'),
    )
    const pixels = drawing_drawingToPixels(tree)
    expect(pixels.length).toBe(4 * 2)
    // row 0: red, red, blue, blue
    expect(pixels[0]).toMatchObject({ red: 255, green: 0, blue: 0, alpha: 255 })
    expect(pixels[1]).toMatchObject({ red: 255, green: 0, blue: 0, alpha: 255 })
    expect(pixels[2]).toMatchObject({ red: 0, green: 0, blue: 255, alpha: 255 })
    expect(pixels[3]).toMatchObject({ red: 0, green: 0, blue: 255, alpha: 255 })
  })
})

// N.B., with-image-from-url is now a Scheme wrapper (image.scm) over the blocking
// primitive image_blockOnFetchImage (SuspendSignal / Scheduler `block-on`), so it
// runs only under the async scheduler with a real browser image loader -- not by
// a direct JS call here.

// N.B., pixel-map is now defined in Scheme (image.scm) on top of image->pixels,
// vector-map, and pixels->image, so it is exercised end-to-end via runProgram in
// image.test.ts rather than by a direct JS call here. The underlying pixel
// round-trip (canvas->pixels / pixels->image / image-get-pixel) is covered below.

describe('pixels->image, image-get-pixel, image->pixels, canvas-set-pixels!', () => {
  // distinct RGBA per cell, row-major: top-left, top-right, bottom-left, bottom-right
  const knownPixels = [
    color_rgb(255, 0, 0, 255),
    color_rgb(0, 255, 0, 128),
    color_rgb(0, 0, 255, 64),
    color_rgb(10, 20, 30, 255),
  ]

  test('pixels->image places each pixel at its row-major position', () => {
    const canvas = canvas_pixelsToCanvas(knownPixels, 2, 2)
    expect(canvas.width).toBe(2)
    expect(canvas.height).toBe(2)
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 1, 0)).toEqual([0, 255, 0, 128])
    expect(pixel(canvas, 0, 1)).toEqual([0, 0, 255, 64])
    expect(pixel(canvas, 1, 1)).toEqual([10, 20, 30, 255])
  })

  test('image-get-pixel reads back the exact value at each coordinate', () => {
    const canvas = canvas_pixelsToCanvas(knownPixels, 2, 2)
    expect(canvas_canvasGetPixel(canvas, 0, 0)).toMatchObject({ red: 255, green: 0, blue: 0, alpha: 255 })
    expect(canvas_canvasGetPixel(canvas, 1, 0)).toMatchObject({ red: 0, green: 255, blue: 0, alpha: 128 })
    expect(canvas_canvasGetPixel(canvas, 0, 1)).toMatchObject({ red: 0, green: 0, blue: 255, alpha: 64 })
    expect(canvas_canvasGetPixel(canvas, 1, 1)).toMatchObject({ red: 10, green: 20, blue: 30, alpha: 255 })
  })

  test('image->pixels flattens the canvas back into the original row-major array', () => {
    const canvas = canvas_pixelsToCanvas(knownPixels, 2, 2)
    const roundTripped = canvas_canvasToPixels(canvas)
    expect(roundTripped).toEqual(knownPixels)
  })

  test('canvas-set-pixels! overwrites an existing canvas in place', () => {
    const canvas = makeCanvas(2, 2)
    canvas_canvasSetPixels(canvas, knownPixels)
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 1, 0)).toEqual([0, 255, 0, 128])
    expect(pixel(canvas, 0, 1)).toEqual([0, 0, 255, 64])
    expect(pixel(canvas, 1, 1)).toEqual([10, 20, 30, 255])
  })

  test('image-get-pixel on a freshly created canvas defaults to transparent black', () => {
    const canvas = makeCanvas(3, 3)
    expect(canvas_canvasGetPixel(canvas, 1, 1)).toMatchObject({ red: 0, green: 0, blue: 0, alpha: 0 })
  })

  test('image->pixels on a freshly created canvas is all transparent black', () => {
    const canvas = makeCanvas(2, 2)
    const pixels = canvas_canvasToPixels(canvas)
    expect(pixels.length).toBe(4)
    pixels.forEach(p => {
      expect(p).toMatchObject({ red: 0, green: 0, blue: 0, alpha: 0 })
    })
  })
})

// These JS functions are correct, but their Scamper bindings can't be reached:
// hsv? is bound to the constructor (#250), breaking every contract that guards
// on it. Calling the functions directly here bypasses that layer and also lets
// colorsys resolve correctly under Vite (see image.test.ts's rgb->hsv skip).
describe('hsv colors (called directly to bypass #250)', () => {
  const c = color_hsv(200, 50, 60, 128)

  test('hsv-hue, hsv-saturation, hsv-value, hsv-alpha read each field', () => {
    expect(color_hsvHue(c)).toBe(200)
    expect(color_hsvSaturation(c)).toBe(50)
    expect(color_hsvValue(c)).toBe(60)
    expect(color_hsvAlpha(c)).toBe(128)
  })

  test('hsv-complement rotates the hue 180 degrees, preserving the other fields', () => {
    const comp = color_hsvComplement(c)
    expect(color_hsvHue(comp)).toBe(20)
    expect(color_hsvSaturation(comp)).toBe(50)
    expect(color_hsvValue(comp)).toBe(60)
    expect(color_hsvAlpha(comp)).toBe(128)
  })

  test('hsv->string formats the components as percentages', () => {
    expect(color_hsvToString(c)).toBe('hsv(200 50%  60% / 50%)')
  })

  test('rgb->hsv converts red to hue 0, full saturation and value', () => {
    const hsv = color_rgbToHsv(color_rgb(255, 0, 0))
    expect(color_hsvHue(hsv)).toBe(0)
    expect(color_hsvSaturation(hsv)).toBe(100)
    expect(color_hsvValue(hsv)).toBe(100)
    expect(color_hsvAlpha(hsv)).toBe(255)
  })

  test('hsv->rgb converts a full-saturation red hue back to rgb red', () => {
    const rgb = color_hsvToRgb(color_hsv(0, 100, 100, 255))
    expect(rgb).toMatchObject({ red: 255, green: 0, blue: 0, alpha: 255 })
  })
})

describe('color_colorToRgb (colour normalization)', () => {
  // colorToRgb accepts an rgba struct, a colour-name string, or an hsv struct.
  // The hsv branch routes through colorsys (which resolves under Vite here but
  // not in the jsdom suite), and the fall-through rejects anything else.
  test('passes an rgba struct through unchanged', () => {
    expect(color_colorToRgb(color_rgb(10, 20, 30))).toMatchObject({
      red: 10, green: 20, blue: 30,
    })
  })
  test('converts a colour-name string', () => {
    expect(color_colorToRgb('red')).toMatchObject({ red: 255, green: 0, blue: 0 })
  })
  test('converts an hsv struct via colorsys', () => {
    expect(color_colorToRgb(color_hsv(0, 100, 100))).toMatchObject({
      red: 255, green: 0, blue: 0,
    })
  })
  test('throws on a value that is not a colour', () => {
    expect(() => color_colorToRgb(42)).toThrow(/valid color/)
  })
})
