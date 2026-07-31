// Runs under real headless Chromium (see test/vitest.browser.config.ts), not
// jsdom, so pixel assertions reflect actual Canvas2D rendering rather than a
// mock. Colors below stick to channel values of 0 or 255 wherever a pixel
// might be partially covered by antialiasing (stroke boundaries): the
// unpremultiplied color of a partially-covered pixel is exact at those
// extremes, but a mid-range channel (e.g. green's 128) can round off by one
// during compositing, so those channels are checked less strictly.
import { describe, expect, test } from 'vitest'
import * as L from '../../src/lpm'
import {
  canvas_canvasCircle,
  canvas_canvasDrawing,
  canvas_canvasEllipse,
  canvas_canvasPath,
  canvas_canvasRectangle,
  canvas_canvasText,
} from '../../src/js/canvas/index.js'
import { image_ellipse } from '../../src/js/image/drawing.js'
import { image_font } from '../../src/js/image/font.js'

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

function hasColorPixel(canvas: HTMLCanvasElement, r: number, g: number, b: number): boolean {
  const data = context2d(canvas).getImageData(0, 0, canvas.width, canvas.height).data
  for (let i = 0; i < data.length; i += 4) {
    if (data[i] === r && data[i + 1] === g && data[i + 2] === b && data[i + 3] > 0) {
      return true
    }
  }
  return false
}

describe('canvas-rectangle!', () => {
  test('solid fills the rectangle and leaves the rest transparent', () => {
    const canvas = makeCanvas(20, 20)
    canvas_canvasRectangle(canvas, 2, 2, 5, 5, 'solid', 'red')
    expect(pixel(canvas, 4, 4)).toEqual([255, 0, 0, 255])
    expect(pixel(canvas, 15, 15)).toEqual([0, 0, 0, 0])
  })

  test('outline strokes the border and leaves the interior transparent', () => {
    const canvas = makeCanvas(20, 20)
    canvas_canvasRectangle(canvas, 2, 2, 10, 10, 'outline', 'red')
    const boundary = pixel(canvas, 2, 6)
    expect(boundary[0]).toBe(255)
    expect(boundary[1]).toBe(0)
    expect(boundary[2]).toBe(0)
    expect(boundary[3]).toBeGreaterThan(0)
    expect(pixel(canvas, 7, 7)).toEqual([0, 0, 0, 0])
  })

  test('invalid mode throws', () => {
    const canvas = makeCanvas(20, 20)
    expect(() => { canvas_canvasRectangle(canvas, 2, 2, 5, 5, 'bogus', 'red') }).toThrow(L.ScamperError)
  })
})

describe('canvas-ellipse!', () => {
  test('solid fills the ellipse and leaves the rest transparent', () => {
    const canvas = makeCanvas(30, 30)
    canvas_canvasEllipse(canvas, 15, 15, 8, 4, 0, 0, 2 * Math.PI, 'solid', 'blue')
    expect(pixel(canvas, 15, 15)).toEqual([0, 0, 255, 255])
    expect(pixel(canvas, 1, 1)).toEqual([0, 0, 0, 0])
  })

  test('outline strokes the boundary and leaves the interior transparent', () => {
    const canvas = makeCanvas(30, 30)
    canvas_canvasEllipse(canvas, 15, 15, 8, 4, 0, 0, 2 * Math.PI, 'outline', 'blue')
    const boundary = pixel(canvas, 22, 15)
    expect(boundary[0]).toBe(0)
    expect(boundary[1]).toBe(0)
    expect(boundary[2]).toBe(255)
    expect(boundary[3]).toBeGreaterThan(0)
    expect(pixel(canvas, 15, 15)).toEqual([0, 0, 0, 0])
  })

  test('invalid mode throws', () => {
    const canvas = makeCanvas(30, 30)
    expect(() => { canvas_canvasEllipse(canvas, 15, 15, 8, 4, 0, 0, 2 * Math.PI, 'bogus', 'blue') }).toThrow(L.ScamperError)
  })
})

describe('canvas-circle!', () => {
  test('solid fills the circle and leaves the rest transparent', () => {
    const canvas = makeCanvas(20, 20)
    canvas_canvasCircle(canvas, 10, 10, 5, 'solid', 'lime')
    expect(pixel(canvas, 10, 10)).toEqual([0, 255, 0, 255])
    expect(pixel(canvas, 1, 1)).toEqual([0, 0, 0, 0])
  })

  test('outline strokes the boundary and leaves the interior transparent', () => {
    const canvas = makeCanvas(20, 20)
    canvas_canvasCircle(canvas, 10, 10, 5, 'outline', 'lime')
    const boundary = pixel(canvas, 15, 10)
    expect(boundary[0]).toBe(0)
    expect(boundary[1]).toBe(255)
    expect(boundary[2]).toBe(0)
    expect(boundary[3]).toBeGreaterThan(0)
    expect(pixel(canvas, 10, 10)).toEqual([0, 0, 0, 0])
  })

  test('invalid mode throws', () => {
    const canvas = makeCanvas(20, 20)
    expect(() => { canvas_canvasCircle(canvas, 10, 10, 5, 'bogus', 'lime') }).toThrow(L.ScamperError)
  })
})

describe('canvas-text!', () => {
  test('solid draws filled glyphs using the default font', () => {
    const canvas = makeCanvas(100, 50)
    canvas_canvasText(canvas, 10, 40, 'W', 30, 'solid', 'red')
    expect(hasColorPixel(canvas, 255, 0, 0)).toBe(true)
  })

  test('solid draws filled glyphs using an explicit font', () => {
    const canvas = makeCanvas(100, 50)
    canvas_canvasText(canvas, 10, 40, 'W', 24, 'solid', 'red', image_font('Georgia', 'serif', true, true))
    expect(hasColorPixel(canvas, 255, 0, 0)).toBe(true)
  })

  test('outline draws stroked glyphs', () => {
    const canvas = makeCanvas(100, 50)
    canvas_canvasText(canvas, 10, 40, 'W', 30, 'outline', 'red')
    expect(hasColorPixel(canvas, 255, 0, 0)).toBe(true)
  })

  test('invalid mode throws', () => {
    const canvas = makeCanvas(100, 50)
    expect(() => { canvas_canvasText(canvas, 10, 40, 'W', 30, 'bogus', 'red') }).toThrow(L.ScamperError)
  })

  test('a non-font extra argument throws', () => {
    const canvas = makeCanvas(100, 50)
    expect(() => { canvas_canvasText(canvas, 10, 40, 'W', 30, 'solid', 'red', 'not-a-font') }).toThrow(L.ScamperError)
  })

  test('2 or more extra arguments throws', () => {
    const canvas = makeCanvas(100, 50)
    expect(() => { canvas_canvasText(canvas, 10, 40, 'W', 30, 'solid', 'red', image_font('Arial'), 'extra') }).toThrow(L.ScamperError)
  })
})

describe('canvas-drawing!', () => {
  test('renders a drawing at the given offset', () => {
    const canvas = makeCanvas(30, 30)
    const drawing = image_ellipse(10, 10, 'solid', 'purple')
    canvas_canvasDrawing(canvas, 5, 5, drawing)
    // image_render centers the ellipse's bounding box at (x + w/2, y + h/2)
    expect(pixel(canvas, 10, 10)).toEqual([128, 0, 128, 255])
    expect(pixel(canvas, 1, 1)).toEqual([0, 0, 0, 0])
  })
})

describe('canvas-path!', () => {
  const trianglePoints = L.mkList(L.mkPair(2, 15), L.mkPair(10, 2), L.mkPair(18, 15))

  test('solid fills the enclosed area and leaves the rest transparent', () => {
    const canvas = makeCanvas(20, 20)
    canvas_canvasPath(canvas, trianglePoints, 'solid', 'yellow')
    expect(pixel(canvas, 10, 12)).toEqual([255, 255, 0, 255])
    expect(pixel(canvas, 1, 1)).toEqual([0, 0, 0, 0])
  })

  test('outline strokes the segments and leaves the interior transparent', () => {
    const canvas = makeCanvas(20, 20)
    canvas_canvasPath(canvas, trianglePoints, 'outline', 'yellow')
    const boundary = pixel(canvas, 6, 7)
    expect(boundary[0]).toBe(255)
    expect(boundary[1]).toBe(255)
    expect(boundary[2]).toBe(0)
    expect(boundary[3]).toBeGreaterThan(0)
    expect(pixel(canvas, 10, 12)).toEqual([0, 0, 0, 0])
  })

  test('invalid mode throws', () => {
    const canvas = makeCanvas(20, 20)
    expect(() => { canvas_canvasPath(canvas, trianglePoints, 'bogus', 'yellow') }).toThrow(L.ScamperError)
  })

  test('an empty point list is a no-op', () => {
    const canvas = makeCanvas(20, 20)
    canvas_canvasPath(canvas, L.mkList(), 'solid', 'yellow')
    expect(pixel(canvas, 10, 10)).toEqual([0, 0, 0, 0])
  })
})

test.skip('animate-with invokes its callback with the current frame time', () => {
  // blocked on https://github.com/slag-plt/scamper/issues/248: L.callScamperFn now unconditionally throws, so the requestAnimationFrame callback can never reach the user's Scamper closure
})

test.skip('canvas-onclick! invokes its callback with the click offset', () => {
  // blocked on https://github.com/slag-plt/scamper/issues/248: L.callScamperFn now unconditionally throws, so the click handler can never reach the user's Scamper closure
})
