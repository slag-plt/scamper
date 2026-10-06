// Regression (#724): a reactive animation flickered. This is the counterpart
// of reactive-canvas-flicker.test.ts, which pins the mechanism against a
// canvas mock; here the whole stack runs -- the real compiler, the real
// scheduler, and real Canvas2D under headless Chromium (see
// test/vitest.browser.config.ts) -- and what is checked is the pixels a
// student would be looking at.
//
// The probe is an animation frame callback registered *after* the component's
// own, so it reads the canvas on screen at the moment the browser is about to
// composite it, having already run the component's frame for that tick. Before
// the fix that frame had cleared the canvas on screen and left the repaint to a
// fiber that cannot run until the next macrotask, so the probe saw a
// transparent canvas -- the flicker, as the compositor sees it.
import { afterEach, beforeAll, describe, expect, test } from 'vitest'
import Scamper, { initialize } from '../../src/scamper'
import * as LPM from '../../src/lpm'
import type { SchedulerId } from '../../src/lpm/scheduler'

// A view that fills the canvas edge to edge, so any complete frame is red
// everywhere and an incomplete one is transparent. The timer keeps the model
// moving, so a frame is drawn for as long as the probe watches.
const PROGRAM = `
(import canvas)
(import reactive)
(display
  (reactive-canvas 40 40 0
    (lambda (st canv) (canvas-rectangle! canv 0 0 40 40 "solid" "red"))
    (lambda (msg st) (+ st 1))
    (on-timer 10)))
`

let runId: SchedulerId | undefined

beforeAll(async () => {
  await initialize()
  // As in empty-program-run.test.ts: scamper.ts registers its renderers as a
  // fire-and-forget module-load side effect, so settle it here rather than
  // letting it land after teardown.
  await import('../../src/app/web/renderers.js')
})

afterEach(() => {
  // Stop the run, and with it the interval its subscription set; otherwise it
  // keeps spawning updates into a torn-down environment.
  if (runId !== undefined) {
    Scamper.getInstance().cancel(runId)
    runId = undefined
  }
})

/** Runs `PROGRAM` and returns the reactive canvas it displayed. */
async function runReactiveCanvas(): Promise<HTMLCanvasElement> {
  const out = new LPM.LoggingChannel(false)
  const req = await Scamper.getInstance().execute({
    src: PROGRAM.trim(),
    out,
    err: out,
  })
  if (req === null) {
    throw new Error('the program did not compile')
  }
  runId = req.id
  await req.done
  const [value] = out.log
  if (!(value instanceof HTMLCanvasElement)) {
    throw new Error(`expected a canvas, got ${JSON.stringify(out.log)}`)
  }
  return value
}

/** Whether every pixel of `canvas` is fully transparent, i.e. nothing is on it. */
function isBlank(canvas: HTMLCanvasElement): boolean {
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new Error('no canvas context')
  }
  const { data } = ctx.getImageData(0, 0, canvas.width, canvas.height)
  for (let i = 3; i < data.length; i += 4) {
    if (data[i] !== 0) {
      return false
    }
  }
  return true
}

/**
 * Watches `canvas` for `count` animation frames.
 *
 * @returns whether each frame found something on the canvas, in order.
 */
function watchFrames(
  canvas: HTMLCanvasElement,
  count: number,
): Promise<boolean[]> {
  return new Promise((resolve) => {
    const painted: boolean[] = []
    const probe = () => {
      painted.push(!isBlank(canvas))
      if (painted.length === count) {
        resolve(painted)
      } else {
        requestAnimationFrame(probe)
      }
    }
    requestAnimationFrame(probe)
  })
}

describe('#724: a reactive animation on screen', () => {
  test('is never blank once its first frame has been drawn', async () => {
    const canvas = await runReactiveCanvas()
    // On screen, so the browser composites it rather than skipping the work.
    document.body.appendChild(canvas)

    const painted = await watchFrames(canvas, 30)

    const first = painted.indexOf(true)
    expect(
      first,
      `no frame was ever painted (${painted.length.toString()} frames watched)`,
    ).not.toBe(-1)
    // Every frame from the first painted one onwards shows a whole frame. A
    // blank one here is the flicker: the canvas cleared with nothing on it.
    expect(painted.slice(first)).toEqual(painted.slice(first).map(() => true))

    canvas.remove()
  })
})
