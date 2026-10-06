// Regression (#724): a reactive animation flickered. `draw` cleared the canvas
// on screen and then spawned the view as a fiber -- and a fiber cannot finish
// inside the frame that started it, since the scheduler yields to the event
// loop before stepping it. So every frame put a cleared canvas on screen and
// painted it only after the browser had composited the blank one.
//
// The invariant here is that the canvas on screen is never cleared with
// nothing drawn on it: the view paints a buffer, and a whole frame is copied
// across in one step.
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import * as L from '../../src/lpm'
import {
  canvas_canvasHeight,
  canvas_canvasQ,
  canvas_canvasRectangle,
  canvas_canvasWidth,
} from '../../src/js/canvas'
import { reactive_onTimer, reactive_reactiveCanvas } from '../../src/js/reactive'

/** A spawned fiber the test has not let run yet. */
type Fiber = () => void

/**
 * Stands in for the run a reactive component spawns into, holding each fiber
 * until the test runs it.
 *
 * The holding is the point: the real scheduler yields to the event loop before
 * stepping a spawned fiber, so the gap between a frame starting and its view
 * finishing is where the flicker was visible.
 */
function stubRun(fibers: Fiber[]): void {
  const controller = new AbortController()
  L.setRunResolver(() => ({
    spawn: (fn, args, onComplete) => {
      fibers.push(() => {
        let result: L.Value | null
        try {
          result = (fn as L.JsFunction)(...args)
        } catch {
          // What the real spawn hands back for a fiber that raised: the error
          // has gone to the program's error channel, and `null` is the result.
          result = null
        }
        onComplete?.(result)
      })
    },
    signal: controller.signal,
  }))
}

/** The 2d calls made on `canvas`, in order, as recorded by vitest-canvas-mock. */
function calls(canvas: HTMLCanvasElement): string[] {
  const ctx = canvas.getContext('2d') as unknown as {
    __getEvents: () => { type: string }[]
  }
  return ctx.__getEvents().map((e) => e.type)
}

describe('a reactive canvas does not flicker', () => {
  let fibers: Fiber[]
  let frames: FrameRequestCallback[]

  /** The view: it paints one rectangle, so a frame is visible in the record. */
  const view = (_st: L.Value, canv: L.Value) => {
    canvas_canvasRectangle(
      canv as HTMLCanvasElement,
      0,
      0,
      10,
      10,
      'solid',
      'red',
    )
    return undefined
  }
  /** The update: a new state every message, so the next frame has work to do. */
  const update = (_msg: L.Value, st: L.Value) => (st as number) + 1

  /** Fires the frame the component asked for, and returns nothing drawn yet. */
  function runFrame(): void {
    const frame = L.shiftRequired(frames, 'an animation frame')
    frame(performance.now())
  }

  /** Runs every fiber spawned so far, as the scheduler eventually would. */
  function runFibers(): void {
    while (fibers.length > 0) {
      L.shiftRequired(fibers, 'a spawned fiber')()
    }
  }

  beforeEach(() => {
    fibers = []
    frames = []
    stubRun(fibers)
    vi.stubGlobal('requestAnimationFrame', (cb: FrameRequestCallback) => {
      frames.push(cb)
      return frames.length
    })
  })

  afterEach(() => {
    vi.unstubAllGlobals()
    vi.useRealTimers()
    L.setRunResolver(() => undefined)
  })

  test('the canvas on screen is untouched while a frame is in flight', () => {
    const canvas = reactive_reactiveCanvas(100, 50, 0, view, update)

    runFrame()

    // The view has been spawned but not run: nothing has been painted, so the
    // canvas on screen must not have been cleared either.
    expect(fibers).toHaveLength(1)
    expect(calls(canvas)).toEqual([])
  })

  test('a finished frame reaches the screen in one step', () => {
    const canvas = reactive_reactiveCanvas(100, 50, 0, view, update)

    runFrame()
    runFibers()

    // Cleared and painted together, with no composite in between.
    expect(calls(canvas)).toEqual(['clearRect', 'drawImage'])
  })

  test('the frame on screen stays there while the next one is drawn', () => {
    // Only the interval: faking requestAnimationFrame too would take the draw
    // loop away from the stub above, which is what drives the frames here.
    vi.useFakeTimers({ toFake: ['setInterval', 'clearInterval'] })
    const canvas = reactive_reactiveCanvas(
      100,
      50,
      0,
      view,
      update,
      reactive_onTimer(10),
    )

    // One whole frame, on screen.
    runFrame()
    runFibers()
    const onScreen = calls(canvas)

    // A tick of the timer, so the next frame has a new state to draw.
    vi.advanceTimersByTime(10)
    runFibers()
    runFrame()

    // The second view is in flight. The first frame is still what the student
    // is looking at, so nothing more may have happened to the canvas on screen.
    expect(fibers).toHaveLength(1)
    expect(calls(canvas)).toEqual(onScreen)

    runFibers()
    expect(calls(canvas)).toEqual([...onScreen, 'clearRect', 'drawImage'])
  })

  // The contention case: a view that needs longer than one frame. Before the
  // fix this was the flicker stretched out -- the frame's draw calls landed on
  // the canvas on screen in instalments, so the frames in between showed
  // partial pictures, and a completed frame could even be cleared by the next
  // frame's draw before it was ever painted. Buffered, a slow view costs frame
  // *rate* and nothing else: the last whole frame stays up, and no second view
  // piles up behind the one still running.
  test('a view that outlives its frame costs frame rate, not the picture', () => {
    vi.useFakeTimers({ toFake: ['setInterval', 'clearInterval'] })
    const canvas = reactive_reactiveCanvas(
      100,
      50,
      0,
      view,
      update,
      reactive_onTimer(10),
    )

    // One whole frame, on screen.
    runFrame()
    runFibers()
    const onScreen = calls(canvas)

    // A tick, then a frame whose view is left running.
    vi.advanceTimersByTime(10)
    runFibers()
    runFrame()
    expect(fibers).toHaveLength(1)

    // Five more frames come and go while that view is still going.
    for (let i = 0; i < 5; i++) {
      runFrame()
    }

    // Still the one view -- frames do not pile up behind it -- and the whole
    // frame from before is still what is on screen.
    expect(fibers).toHaveLength(1)
    expect(calls(canvas)).toEqual(onScreen)

    // It finishes, and exactly one new frame reaches the screen.
    runFibers()
    expect(calls(canvas)).toEqual([...onScreen, 'clearRect', 'drawImage'])
  })

  // A view that fails half way through still has what it managed to paint put
  // on screen, which is what the old code did implicitly by painting the
  // canvas on screen directly. The error itself goes to the output pane, and
  // the animation stops rather than spinning on a view that cannot run.
  test('a view that errors shows what it drew and stops the animation', () => {
    const failing = (st: L.Value, canv: L.Value) => {
      view(st, canv)
      throw new L.ScamperError('Runtime', 'the view gave up')
    }
    const canvas = reactive_reactiveCanvas(100, 50, 0, failing, update)

    runFrame()
    runFibers()

    expect(calls(canvas)).toEqual(['clearRect', 'drawImage'])

    // Stopped: the frame already requested starts no view, and the draw loop
    // asks for no further ones.
    runFrame()
    expect(fibers).toEqual([])
    expect(frames).toEqual([])
  })

  // The view is handed the buffer rather than the canvas on screen, so what it
  // gets has to still be a canvas of the same size: `canvas?` guards every
  // drawing procedure in canvas.scm, and canvas-width/canvas-height have to
  // agree with what the student asked for.
  test('what the view is handed is a canvas of the declared size', () => {
    const handed: L.Value[] = []
    const record = (_st: L.Value, canv: L.Value) => {
      handed.push(canv)
      return undefined
    }
    const canvas = reactive_reactiveCanvas(100, 50, 0, record, update)

    runFrame()
    runFibers()

    const [buffer] = handed
    expect(canvas_canvasQ(buffer)).toBe(true)
    expect(canvas_canvasWidth(buffer)).toBe(100)
    expect(canvas_canvasHeight(buffer)).toBe(50)
    // Off screen, which is the whole point: it is not the canvas being shown.
    expect(buffer).not.toBe(canvas)
    expect(document.body.contains(buffer as HTMLCanvasElement)).toBe(false)
  })
})
