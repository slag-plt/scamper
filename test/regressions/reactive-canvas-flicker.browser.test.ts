// Regression (#724): a reactive animation flickered. This is the counterpart
// of reactive-canvas-flicker.test.ts, which pins the mechanism against a
// canvas mock; here the whole stack runs -- the real compiler, the real
// scheduler, and real Canvas2D under headless Chromium (see
// test/vitest.browser.config.ts). What a mock cannot answer is checked here:
// the pixels a student would be looking at, and the two real Canvas2D
// behaviours the fix depends on.
//
// The probe is an animation frame callback registered *after* the component's
// own, so each frame it reads the canvas having already run the component's
// frame for that tick -- which is what the browser is about to composite.
// Before the fix that frame had cleared the canvas on screen and left the
// repaint to a fiber that cannot run until the next macrotask, so the probe saw
// a transparent canvas: the flicker, as the compositor sees it.
import { afterEach, beforeAll, describe, expect, test } from 'vitest'
import Scamper, { initialize } from '../../src/scamper'
import * as LPM from '../../src/lpm'
import type { SchedulerId } from '../../src/lpm/scheduler'

/** A view that fills the canvas edge to edge, so a complete frame is red
 *  everywhere and an incomplete one is transparent. The timer keeps the model
 *  moving, so a frame is drawn for as long as the probe watches. */
function program(width: number, height: number): string {
  return `
(import canvas)
(import reactive)
(display
  (reactive-canvas ${width.toString()} ${height.toString()} 0
    (lambda (st canv)
      (canvas-rectangle! canv 0 0 ${width.toString()} ${height.toString()} "solid" "red"))
    (lambda (msg st) (+ st 1))
    (on-timer 10)))
`.trim()
}

/** How long the frame-watching tests may take. Deliberately generous: a fixed
 *  number of frames on a loaded CI runner with a software rasteriser is work of
 *  unbounded duration, and a timeout is a hang detector rather than a
 *  performance assertion about the machine (#536). */
const BUDGET_MS = 20_000

const runs: SchedulerId[] = []
const shown: HTMLCanvasElement[] = []

beforeAll(async () => {
  await initialize()
  // As in empty-program-run.test.ts: scamper.ts registers its renderers as a
  // fire-and-forget module-load side effect, so settle it here rather than
  // letting it land after teardown.
  await import('../../src/app/web/renderers.js')
})

afterEach(() => {
  // Stop each run, and with it the interval its subscription set; otherwise it
  // keeps spawning updates into a torn-down environment. This config runs
  // files serially over one browser, so a leaked timer would reach the next.
  while (runs.length > 0) {
    const id = runs.pop()
    if (id !== undefined) {
      Scamper.getInstance().cancel(id)
    }
  }
  while (shown.length > 0) {
    shown.pop()?.remove()
  }
})

/** Runs `src`, returning its output channel once the program has settled. */
async function run(src: string): Promise<LPM.LoggingChannel> {
  // combineLogs off, so `log` holds only displayed values -- the canvas below
  // is read straight out of it -- and a reported error is in `errLog`, where a
  // failing test can name it rather than printing a canvas as `{}`.
  const out = new LPM.LoggingChannel(false, false)
  const req = await Scamper.getInstance().execute({ src, out, err: out })
  if (req === null) {
    throw new Error(`the program did not compile: ${out.errLog.join('; ')}`)
  }
  runs.push(req.id)
  await req.done
  return out
}

/** Runs a reactive-canvas program and returns the canvas it displayed. */
async function runReactiveCanvas(
  width: number,
  height: number,
): Promise<HTMLCanvasElement> {
  const out = await run(program(width, height))
  const [value] = out.log
  if (!(value instanceof HTMLCanvasElement)) {
    throw new Error(`expected a canvas; errors: ${out.errLog.join('; ')}`)
  }
  document.body.appendChild(value)
  shown.push(value)
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
  test(
    'is never blank once its first frame has been drawn',
    async () => {
      const canvas = await runReactiveCanvas(40, 40)

      const painted = await watchFrames(canvas, 10)

      const first = painted.indexOf(true)
      expect(
        first,
        `no frame was ever painted (${painted.length.toString()} frames watched)`,
      ).not.toBe(-1)
      // Every frame from the first painted one onwards shows a whole frame. A
      // blank one here is the flicker: the canvas cleared with nothing on it.
      expect(painted.slice(first)).toEqual(
        Array.from({ length: painted.length - first }, () => true),
      )
    },
    BUDGET_MS,
  )
})

// Why `present` checks for a zero dimension before blitting. Both halves need a
// real Canvas2D: the mock neither throws here nor reproduces the consequence.
describe('a reactive canvas of zero size', () => {
  test('is a thing real Canvas2D refuses to blit', () => {
    const front = document.createElement('canvas')
    front.width = 0
    front.height = 40
    const ctx = front.getContext('2d')
    if (ctx === null) {
      throw new Error('no canvas context')
    }
    // Per the usability check in the canvas spec, a zero-dimension canvas
    // source is an InvalidStateError rather than a silent no-op. If this ever
    // stops throwing, the guard in `present` can go.
    expect(() => {
      ctx.drawImage(front, 0, 0)
    }).toThrow()
  })

  // `reactive-canvas` takes any `number?`, so this is a legal program -- and a
  // throw out of a view's completion callback would land in the scheduler's
  // own loop, which has no onFatal for a spawned fiber and so retires the loop
  // and takes every later run with it.
  test(
    'draws nothing and leaves the scheduler working',
    async () => {
      const out = await run(program(0, 40))
      expect(out.errLog).toEqual([])

      // Let several frames go by, each of which would try to blit.
      await new Promise<void>((resolve) => {
        let frames = 0
        const tick = () => {
          frames++
          if (frames === 5) {
            resolve()
          } else {
            requestAnimationFrame(tick)
          }
        }
        requestAnimationFrame(tick)
      })

      // The scheduler is still alive, which it would not be had the blit thrown.
      const after = await run('(+ 1 1)')
      expect(after.errLog).toEqual([])
      expect(after.log).toEqual([2])
    },
    BUDGET_MS,
  )
})
