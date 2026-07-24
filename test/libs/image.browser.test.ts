// Runs under real headless Chromium (see test/vitest.browser.config.ts), not
// jsdom, so pixel and font-metric assertions reflect actual Canvas2D
// behavior rather than a mock. Two real-browser wrinkles worth knowing, both
// artifacts of internal premultiplied-alpha storage: (1) putImageData then
// getImageData loses RGB entirely for a fully transparent pixel -- it always
// reads back as (0, 0, 0, 0); (2) for a partially transparent pixel, a
// mid-range color channel can round off by one. So the known pixel arrays
// below only use non-zero alpha, and only use mid-range color channels
// alongside fully opaque (alpha 255) pixels, where premultiplication is exact.
import { describe, expect, test, vi } from 'vitest'
import * as L from '../../src/lpm'
import {
  image_colorToRgb,
  image_hsv,
  image_hsvAlpha,
  image_hsvComplement,
  image_hsvHue,
  image_hsvSaturation,
  image_hsvToRgb,
  image_hsvToString,
  image_hsvValue,
  image_rgb,
  image_rgbToHsv,
} from '../../src/js/image/color.js'
import {
  image_above,
  image_beside,
  image_drawingToImage,
  image_drawingToPixels,
  image_ellipse,
  image_isoscelesTriangle,
  image_overlay,
  image_overlayOffset,
  image_path,
  image_rectangle,
  image_rotate,
  image_text,
  image_withDash,
} from '../../src/js/image/drawing.js'
import {
  image_canvasSetPixels,
  image_imageGetPixel,
  image_imageToPixels,
  image_pixelMap,
  image_pixelsToImage,
  image_withImageFromUrl,
} from '../../src/js/image/image.js'

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

// True if any pixel departs from the opaque-white background image_clearDrawing
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

// A no-op stand-in for a Scamper closure; L.callScamperFn throws before ever
// looking at it (see #248), so its actual behavior doesn't matter here.
const dummyScamperFn = ((..._args: L.Value[]) => undefined) as L.ScamperFn

// 1x1 red, fully-opaque PNG, built by hand (no image-authoring tool in this repo).
const RED_PIXEL_PNG = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGP4z8DwHwAFAAH/iZk9HQAAAABJRU5ErkJggg=='

describe('text', () => {
  test('reports positive width and height', () => {
    const t = image_text('hello', 20, image_rgb(0, 0, 0, 255))
    expect(t.width).toBeGreaterThan(0)
    expect(t.height).toBeGreaterThan(0)
  })

  test('longer text is wider', () => {
    const short = image_text('a', 20, image_rgb(0, 0, 0, 255))
    const long = image_text('a much longer piece of text', 20, image_rgb(0, 0, 0, 255))
    expect(long.width).toBeGreaterThan(short.width)
  })

  test('a larger size produces a larger width and height', () => {
    const small = image_text('hello', 10, image_rgb(0, 0, 0, 255))
    const big = image_text('hello', 40, image_rgb(0, 0, 0, 255))
    expect(big.width).toBeGreaterThan(small.width)
    expect(big.height).toBeGreaterThan(small.height)
  })

  test('empty text has zero width', () => {
    const t = image_text('', 20, image_rgb(0, 0, 0, 255))
    expect(t.width).toBe(0)
  })
})

describe('drawing->image', () => {
  test('renders a single shape to a canvas of the drawing size', () => {
    const canvas = image_drawingToImage(image_rectangle(2, 2, 'solid', 'red'))
    expect(canvas.width).toBe(2)
    expect(canvas.height).toBe(2)
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 1, 1)).toEqual([255, 0, 0, 255])
  })

  test('renders a composite drawing tree, positioning each subdrawing', () => {
    const tree = image_beside(
      image_rectangle(2, 2, 'solid', 'red'),
      image_rectangle(2, 2, 'solid', 'blue'),
    )
    const canvas = image_drawingToImage(tree)
    expect(canvas.width).toBe(4)
    expect(canvas.height).toBe(2)
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 3, 1)).toEqual([0, 0, 255, 255])
  })
})

// One test per image_render branch, driven end-to-end through a real Canvas2D.
// rectangle and beside are already exercised by drawing->image above.
describe('render per-shape branches', () => {
  test('ellipse fills its interior', () => {
    const canvas = image_drawingToImage(image_ellipse(10, 10, 'solid', 'red'))
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(10)
    expect(pixel(canvas, 5, 5)).toEqual([255, 0, 0, 255])
  })

  test('triangle fills its interior', () => {
    const canvas = image_drawingToImage(image_isoscelesTriangle(10, 10, 'solid', 'blue'))
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(10)
    expect(pixel(canvas, 5, 8)).toEqual([0, 0, 255, 255])
  })

  test('path fills the polygon it traces', () => {
    const points = L.mkList(
      L.mkPair(0, 0), L.mkPair(10, 0), L.mkPair(10, 10), L.mkPair(0, 10),
    )
    const canvas = image_drawingToImage(image_path(10, 10, points, 'solid', 'green'))
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(10)
    // green is (0, 128, 0); alpha 255 keeps the mid-range channel exact
    expect(pixel(canvas, 5, 5)).toEqual([0, 128, 0, 255])
  })

  test('ellipse outline strokes its boundary', () => {
    const canvas = image_drawingToImage(image_ellipse(10, 10, 'outline', 'red'))
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(10)
    expect(hasColoredPixel(canvas)).toBe(true)
  })

  test('triangle outline strokes its edges', () => {
    const canvas = image_drawingToImage(image_isoscelesTriangle(10, 10, 'outline', 'blue'))
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(10)
    expect(hasColoredPixel(canvas)).toBe(true)
  })

  test('path outline strokes the polyline it traces', () => {
    const points = L.mkList(L.mkPair(1, 1), L.mkPair(8, 1), L.mkPair(4, 8))
    const canvas = image_drawingToImage(image_path(10, 10, points, 'outline', 'green'))
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(10)
    expect(hasColoredPixel(canvas)).toBe(true)
  })

  test('above stacks subdrawings vertically', () => {
    const canvas = image_drawingToImage(image_above(
      image_rectangle(4, 4, 'solid', 'red'),
      image_rectangle(4, 4, 'solid', 'blue'),
    ))
    expect(canvas.width).toBe(4)
    expect(canvas.height).toBe(8)
    expect(pixel(canvas, 2, 1)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 2, 6)).toEqual([0, 0, 255, 255])
  })

  test('overlay draws the first drawing on top of the rest', () => {
    const canvas = image_drawingToImage(image_overlay(
      image_rectangle(4, 4, 'solid', 'blue'),
      image_rectangle(8, 8, 'solid', 'red'),
    ))
    expect(canvas.width).toBe(8)
    expect(canvas.height).toBe(8)
    expect(pixel(canvas, 4, 4)).toEqual([0, 0, 255, 255])
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
  })

  test('overlay/offset shifts the second drawing and keeps the first on top', () => {
    const canvas = image_drawingToImage(image_overlayOffset(
      2, 2,
      image_rectangle(4, 4, 'solid', 'red'),
      image_rectangle(4, 4, 'solid', 'blue'),
    ))
    expect(canvas.width).toBe(6)
    expect(canvas.height).toBe(6)
    expect(pixel(canvas, 1, 1)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 5, 5)).toEqual([0, 0, 255, 255])
  })

  test('rotate fills the interior of its grown bounding box', () => {
    const canvas = image_drawingToImage(image_rotate(90, image_rectangle(10, 20, 'solid', 'red')))
    expect(canvas.width).toBeGreaterThan(0)
    expect(canvas.height).toBeGreaterThan(0)
    expect(pixel(canvas, 10, 5)).toEqual([255, 0, 0, 255])
  })

  test('with-dash strokes a dashed outline', () => {
    const canvas = image_drawingToImage(image_withDash([4, 4], image_rectangle(20, 20, 'outline', 'red')))
    expect(canvas.width).toBe(20)
    expect(canvas.height).toBe(20)
    expect(hasColoredPixel(canvas)).toBe(true)
  })

  test('text draws visible ink on the canvas', () => {
    const canvas = image_drawingToImage(image_text('H', 40, image_rgb(0, 0, 0, 255)))
    expect(canvas.width).toBeGreaterThan(0)
    expect(canvas.height).toBeGreaterThan(0)
    expect(hasColoredPixel(canvas)).toBe(true)
  })
})

describe('drawing->pixels', () => {
  test('flattens a rendered drawing into a row-major Rgb array', () => {
    const tree = image_beside(
      image_rectangle(2, 2, 'solid', 'red'),
      image_rectangle(2, 2, 'solid', 'blue'),
    )
    const pixels = image_drawingToPixels(tree)
    expect(pixels.length).toBe(4 * 2)
    // row 0: red, red, blue, blue
    expect(pixels[0]).toMatchObject({ red: 255, green: 0, blue: 0, alpha: 255 })
    expect(pixels[1]).toMatchObject({ red: 255, green: 0, blue: 0, alpha: 255 })
    expect(pixels[2]).toMatchObject({ red: 0, green: 0, blue: 255, alpha: 255 })
    expect(pixels[3]).toMatchObject({ red: 0, green: 0, blue: 255, alpha: 255 })
  })
})

describe('with-image-from-url', () => {
  test('synchronously returns a loading placeholder before the image loads', () => {
    const url = 'https://example.com/some-image.png'
    const container = image_withImageFromUrl(url, dummyScamperFn)
    expect(container.innerHTML).toBe(`Loading ${url}...`)
  })

  test('loading a real same-origin image fires onload and surfaces the #248 callScamperFn error', async () => {
    const container = image_withImageFromUrl(RED_PIXEL_PNG, dummyScamperFn)
    await vi.waitFor(() => {
      expect(container.innerHTML).not.toBe(`Loading ${RED_PIXEL_PNG}...`)
    })
    // blocked on https://github.com/slag-plt/scamper/issues/248: the onload handler
    // calls L.callScamperFn, which always throws; that error is caught and rendered
    expect(container.textContent).toContain('Javascript library functions can no longer call Scamper functions')
  })
})

describe('pixel-map', () => {
  test('throws the #248 callScamperFn error on a canvas of the given dimensions', () => {
    const canvas = makeCanvas(3, 5)
    // pixel-map builds its output canvas at these same dimensions (see image.ts) --
    // but it never gets there because the very first pixel's callback throws (#248)
    expect(canvas.width).toBe(3)
    expect(canvas.height).toBe(5)
    expect(() => image_pixelMap(dummyScamperFn, canvas)).toThrow(L.ScamperError)
    expect(() => image_pixelMap(dummyScamperFn, canvas)).toThrow('Javascript library functions can no longer call Scamper functions')
  })

  test.skip('applies the callback to transform each pixel', () => {
    // blocked on https://github.com/slag-plt/scamper/issues/248: L.callScamperFn now always throws, so pixel-map's per-pixel callback can never run end-to-end
  })
})

describe('pixels->image, image-get-pixel, image->pixels, canvas-set-pixels!', () => {
  // distinct RGBA per cell, row-major: top-left, top-right, bottom-left, bottom-right
  const knownPixels = [
    image_rgb(255, 0, 0, 255),
    image_rgb(0, 255, 0, 128),
    image_rgb(0, 0, 255, 64),
    image_rgb(10, 20, 30, 255),
  ]

  test('pixels->image places each pixel at its row-major position', () => {
    const canvas = image_pixelsToImage(knownPixels, 2, 2)
    expect(canvas.width).toBe(2)
    expect(canvas.height).toBe(2)
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 1, 0)).toEqual([0, 255, 0, 128])
    expect(pixel(canvas, 0, 1)).toEqual([0, 0, 255, 64])
    expect(pixel(canvas, 1, 1)).toEqual([10, 20, 30, 255])
  })

  test('image-get-pixel reads back the exact value at each coordinate', () => {
    const canvas = image_pixelsToImage(knownPixels, 2, 2)
    expect(image_imageGetPixel(canvas, 0, 0)).toMatchObject({ red: 255, green: 0, blue: 0, alpha: 255 })
    expect(image_imageGetPixel(canvas, 1, 0)).toMatchObject({ red: 0, green: 255, blue: 0, alpha: 128 })
    expect(image_imageGetPixel(canvas, 0, 1)).toMatchObject({ red: 0, green: 0, blue: 255, alpha: 64 })
    expect(image_imageGetPixel(canvas, 1, 1)).toMatchObject({ red: 10, green: 20, blue: 30, alpha: 255 })
  })

  test('image->pixels flattens the canvas back into the original row-major array', () => {
    const canvas = image_pixelsToImage(knownPixels, 2, 2)
    const roundTripped = image_imageToPixels(canvas)
    expect(roundTripped).toEqual(knownPixels)
  })

  test('canvas-set-pixels! overwrites an existing canvas in place', () => {
    const canvas = makeCanvas(2, 2)
    image_canvasSetPixels(canvas, knownPixels)
    expect(pixel(canvas, 0, 0)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 1, 0)).toEqual([0, 255, 0, 128])
    expect(pixel(canvas, 0, 1)).toEqual([0, 0, 255, 64])
    expect(pixel(canvas, 1, 1)).toEqual([10, 20, 30, 255])
  })

  test('image-get-pixel on a freshly created canvas defaults to transparent black', () => {
    const canvas = makeCanvas(3, 3)
    expect(image_imageGetPixel(canvas, 1, 1)).toMatchObject({ red: 0, green: 0, blue: 0, alpha: 0 })
  })

  test('image->pixels on a freshly created canvas is all transparent black', () => {
    const canvas = makeCanvas(2, 2)
    const pixels = image_imageToPixels(canvas)
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
  const c = image_hsv(200, 50, 60, 128)

  test('hsv-hue, hsv-saturation, hsv-value, hsv-alpha read each field', () => {
    expect(image_hsvHue(c)).toBe(200)
    expect(image_hsvSaturation(c)).toBe(50)
    expect(image_hsvValue(c)).toBe(60)
    expect(image_hsvAlpha(c)).toBe(128)
  })

  test('hsv-complement rotates the hue 180 degrees, preserving the other fields', () => {
    const comp = image_hsvComplement(c)
    expect(image_hsvHue(comp)).toBe(20)
    expect(image_hsvSaturation(comp)).toBe(50)
    expect(image_hsvValue(comp)).toBe(60)
    expect(image_hsvAlpha(comp)).toBe(128)
  })

  test('hsv->string formats the components as percentages', () => {
    expect(image_hsvToString(c)).toBe('hsv(200 50%  60% / 50%)')
  })

  test('rgb->hsv converts red to hue 0, full saturation and value', () => {
    const hsv = image_rgbToHsv(image_rgb(255, 0, 0))
    expect(image_hsvHue(hsv)).toBe(0)
    expect(image_hsvSaturation(hsv)).toBe(100)
    expect(image_hsvValue(hsv)).toBe(100)
    expect(image_hsvAlpha(hsv)).toBe(255)
  })

  test('hsv->rgb converts a full-saturation red hue back to rgb red', () => {
    const rgb = image_hsvToRgb(image_hsv(0, 100, 100, 255))
    expect(rgb).toMatchObject({ red: 255, green: 0, blue: 0, alpha: 255 })
  })
})

describe('image_colorToRgb (colour normalization)', () => {
  // colorToRgb accepts an rgba struct, a colour-name string, or an hsv struct.
  // The hsv branch routes through colorsys (which resolves under Vite here but
  // not in the jsdom suite), and the fall-through rejects anything else.
  test('passes an rgba struct through unchanged', () => {
    expect(image_colorToRgb(image_rgb(10, 20, 30))).toMatchObject({
      red: 10, green: 20, blue: 30,
    })
  })
  test('converts a colour-name string', () => {
    expect(image_colorToRgb('red')).toMatchObject({ red: 255, green: 0, blue: 0 })
  })
  test('converts an hsv struct via colorsys', () => {
    expect(image_colorToRgb(image_hsv(0, 100, 100))).toMatchObject({
      red: 255, green: 0, blue: 0,
    })
  })
  test('throws on a value that is not a colour', () => {
    expect(() => image_colorToRgb(42)).toThrow(/valid color/)
  })
})
