import { describe, expect, test, vi } from 'vitest'
import { runProgram } from './harness.js'
import { canvas_animateWith, canvas_canvasOnclick, canvas_makeCanvas } from '../../src/js/canvas/index.js'

// canvas.scm binds directly to the Canvas2D API. The functions below don't
// need real Canvas2D rendering to test, so they're covered here. The actual
// drawing calls (canvas-rectangle!, etc.) need a browser-API mocking
// strategy that is a separate, larger effort -- left as placeholders.

describe('canvas?', () => {
  test('is true for a value made by make-canvas', async () => {
    expect(await runProgram(`
    (import canvas)
    (canvas? (make-canvas 10 10))
    `)).toEqual(['#t'])
  })

  test('is false for non-canvas values', async () => {
    expect(await runProgram(`
    (import canvas)
    (canvas? 5)
    (canvas? "canvas")
    (canvas? #t)
    (canvas? (list 1 2 3))
    (canvas? (pair 1 2))
    `)).toEqual(['#f', '#f', '#f', '#f', '#f'])
  })
})

describe('make-canvas', () => {
  test('creates a canvas element with the given width and height', () => {
    const canvas = canvas_makeCanvas(10, 20)
    expect(canvas).toBeInstanceOf(HTMLCanvasElement)
    expect(canvas.width).toBe(10)
    expect(canvas.height).toBe(20)
  })

  test('a different width and height produce a differently-sized canvas', () => {
    const canvas = canvas_makeCanvas(100, 50)
    expect(canvas.width).toBe(100)
    expect(canvas.height).toBe(50)
  })
})

describe('animate-with', () => {
  test('schedules a frame via requestAnimationFrame without throwing', () => {
    if (typeof window.requestAnimationFrame !== 'function') {
      // jsdom in this environment doesn't implement requestAnimationFrame; nothing to assert.
      return
    }
    // Mock the implementation so the scheduled callback never actually
    // fires -- invoking it would call the Scamper callback via
    // L.callScamperFn, which is the blocked path (see the skip below).
    const raf = vi.spyOn(window, 'requestAnimationFrame').mockImplementation(() => 0)
    expect(() => { canvas_animateWith(() => true) }).not.toThrow()
    expect(raf).toHaveBeenCalledOnce()
    raf.mockRestore()
  })

  // L.callScamperFn (src/lpm/lang.ts) now unconditionally throws, so
  // animate-with's callback invocation can't be tested end-to-end (#248).
  test.skip('invokes the Scamper callback with the current time')
})

describe('canvas-onclick!', () => {
  test('registers a click listener on the canvas', () => {
    const canvas = canvas_makeCanvas(10, 10)
    const spy = vi.spyOn(canvas, 'addEventListener')
    canvas_canvasOnclick(canvas, () => undefined)
    // Registered via addEventListener (with the run's AbortSignal in the options
    // object) so the listener is torn down when the program is re-run/stopped.
    expect(spy).toHaveBeenCalledWith(
      'click',
      expect.any(Function),
      expect.any(Object),
    )
    spy.mockRestore()
  })

  // L.callScamperFn (src/lpm/lang.ts) now unconditionally throws, so
  // canvas-onclick!'s callback invocation can't be tested end-to-end (#248).
  test.skip('invokes the Scamper callback with click coordinates')
})

// Regression: color? and image? are used in canvas.scm's drawing-function
// contracts but defined in image.scm. canvas.scm must re-export them; otherwise
// every drawing call runs `(color? ...)` / `(drawing? ...)` against an unbound
// name and throws "Variable not found" unless the user also imports image.
describe('cross-module predicates (color?, image?) resolve with only canvas imported', () => {
  test('color? and image? are in scope', async () => {
    expect(await runProgram(`
    (import canvas)
    (color? "red")
    (color? 5)
    (drawing? 5)
    `)).toEqual(['#t', '#f', '#f'])
  })

  test('canvas-rectangle! contract fires cleanly on a bad color', async () => {
    const out = (await runProgram(`
    (import canvas)
    (canvas-rectangle! (make-canvas 10 10) 0 0 5 5 "solid" 42)
    `)).join('\n')
    expect(out).toContain('expected a color')
    expect(out).not.toContain('Variable not found')
  })

  // canvas-text!'s font is an optional parameter rather than a rest parameter
  // (#446), so the contract checks both its type and the call's arity. Before
  // that the parameter was declared `string?` while the implementation wanted a
  // font, so no font argument at all could get through. The first line below
  // also guards canvas.scm's re-export of `font?`: without it the contract's
  // predicate does not resolve and this fails with `Variable not found`.
  test('canvas-text! takes a font optionally and checks it', async () => {
    expect(await runProgram(`
    (import canvas)
    (canvas-text! (make-canvas 10 10) 0 0 "hi" 12 "solid" "red" 5)
    `)).toEqual(['Runtime error: (error) expected a font, received number'])
  })

  test('canvas-text! accepts a font and rejects a ninth argument', async () => {
    expect(await runProgram(`
    (import canvas)
    (import image)
    (canvas-text! (make-canvas 10 10) 0 0 "hi" 12 "solid" "red" (font "Georgia" "serif" #t #f))
    (canvas-text! (make-canvas 10 10) 0 0 "hi" 12 "solid" "red" (font "Arial") 9)
    `)).toEqual([
      'void',
      'Runtime error: (canvas-text!) Arity mismatch in function call: expected at most 8 arguments, got 9',
    ])
  })
})

test.todo('canvas-ellipse!')
test.todo('canvas-circle!')
test.todo('canvas-text!')
test.todo('canvas-drawing!')
test.todo('canvas-path!')
