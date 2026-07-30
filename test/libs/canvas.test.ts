import { describe, expect, test, vi } from 'vitest'
import { runProgram } from '../harness.js'
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
  test('registers an onclick handler on the canvas', () => {
    const canvas = canvas_makeCanvas(10, 10)
    expect(canvas.onclick).toBeNull()
    canvas_canvasOnclick(canvas, () => undefined)
    expect(typeof canvas.onclick).toBe('function')
  })

  // L.callScamperFn (src/lpm/lang.ts) now unconditionally throws, so
  // canvas-onclick!'s callback invocation can't be tested end-to-end (#248).
  test.skip('invokes the Scamper callback with click coordinates')
})

test.todo('canvas-rectangle!')
test.todo('canvas-ellipse!')
test.todo('canvas-circle!')
test.todo('canvas-text!')
test.todo('canvas-drawing!')
test.todo('canvas-path!')
