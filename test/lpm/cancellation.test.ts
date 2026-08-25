import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { setRunResolver, Value } from '../../src/lpm'
import { html_button, html_onKeydown } from '../../src/js/html/index.js'
import { canvas_animateWith, canvas_canvasOnclick, canvas_makeCanvas } from '../../src/js/canvas/index.js'
import { reactive_onTimer } from '../../src/js/reactive/index.js'

// Re-running (or stopping) a program aborts its run's AbortSignal, and every
// long-lived event handler captures that signal at registration to tear itself
// down. These tests drive the library handlers with a fake `spawn` (so no
// Scamper program is needed) and a controllable signal, then abort and check the
// handler no longer fires -- i.e. the previous run's callbacks don't leak.

// eslint-disable-next-line @typescript-eslint/no-explicit-any
type FakeElement = any

let controller: AbortController
let spawned: Value[][]

beforeEach(() => {
  controller = new AbortController()
  spawned = []
  // One fake run. Record spawns; drive the frame loop by reporting #t from
  // each callback.
  setRunResolver(() => ({
    spawn: (_fn, args, onComplete) => {
      spawned.push(args)
      onComplete?.(true)
    },
    signal: controller.signal,
  }))
})

afterEach(() => {
  controller.abort() // remove any listeners this test left on window/document
  vi.restoreAllMocks()
})

describe('run cancellation', () => {
  test('on-keydown! stops firing after the run is aborted', () => {
    html_onKeydown(() => undefined)
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'a' }))
    expect(spawned.map((args) => args[0])).toEqual(['a'])

    controller.abort()
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'b' }))
    // The 'b' keydown must not spawn: the listener was removed on abort.
    expect(spawned.map((args) => args[0])).toEqual(['a'])
  })

  test('button stops firing after the run is aborted', () => {
    const b = html_button('Click me', () => undefined)
    b.dispatchEvent(new MouseEvent('click'))
    expect(spawned.length).toBe(1)

    controller.abort()
    b.dispatchEvent(new MouseEvent('click'))
    expect(spawned.length).toBe(1)
  })

  test('canvas-onclick! stops firing after the run is aborted', () => {
    const canvas = canvas_makeCanvas(10, 10)
    canvas_canvasOnclick(canvas, () => undefined)
    canvas.dispatchEvent(new MouseEvent('click'))
    expect(spawned.length).toBe(1)

    controller.abort()
    canvas.dispatchEvent(new MouseEvent('click'))
    expect(spawned.length).toBe(1)
  })

  test('on-timer clears its interval when the run is aborted', () => {
    vi.useFakeTimers()
    const updates: Value[] = []
    const react: FakeElement = {
      update: (msg: Value) => updates.push(msg),
      draw: () => undefined,
      getElement: () => document.createElement('div'),
    }
    reactive_onTimer(50).register(react)

    vi.advanceTimersByTime(120) // ~2 ticks
    const before = updates.length
    expect(before).toBeGreaterThanOrEqual(2)

    controller.abort()
    vi.advanceTimersByTime(200)
    // No further ticks: the interval was cleared on abort.
    expect(updates.length).toBe(before)
    vi.useRealTimers()
  })

  test('animate-with stops requesting frames after the run is aborted', () => {
    const pending: FrameRequestCallback[] = []
    vi.spyOn(window, 'requestAnimationFrame').mockImplementation((cb) => {
      pending.push(cb)
      return pending.length
    })

    canvas_animateWith(() => undefined)
    // Drain a few frames; each spawns (fake reports #t) and re-arms the loop.
    for (let i = 0; i < 3 && pending.length > 0; i++) {
      pending.shift()!(i)
    }
    const before = spawned.length
    expect(before).toBe(3)

    controller.abort()
    // Any already-queued frame callback must early-return: no spawn, no re-arm.
    while (pending.length > 0) {
      pending.shift()!(99)
    }
    expect(spawned.length).toBe(before)
  })
})
