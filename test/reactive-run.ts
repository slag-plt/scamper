/**
 * A stand-in for the run that a reactive component spawns its update and view
 * fibers into, shared by the specs about those components.
 *
 * The point is that a fiber is *held* until the test lets it run. The real
 * scheduler yields to the event loop before stepping a spawned fiber
 * (`schedulerYield` in src/lpm/scheduler.ts), so a component is always in the
 * gap between spawning a fiber and seeing its result -- and that gap is where
 * both #724 (a frame cleared but not yet painted) and #725 (messages arriving
 * behind an update that has not finished) are visible. Holding the fiber is
 * what makes the gap long enough to look at.
 *
 * Kept out of test/util.ts, which reaches for node:process and so cannot be
 * imported by a browser spec.
 */
import * as L from '../src/lpm'

/** A spawned fiber the test has not let run yet. */
export type Fiber = () => void

/**
 * Points the run resolver at a stub that collects spawned fibers into `fibers`
 * instead of running them.
 *
 * @returns the run's AbortController, so a test can stop it the way the IDE's
 *          Stop button does.
 */
export function stubRun(fibers: Fiber[]): AbortController {
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
  return controller
}

/** Forgets the stub, so a later test does not spawn into a dead run. */
export function clearRun(): void {
  L.setRunResolver(() => undefined)
}

/**
 * Runs every fiber held so far, as the scheduler eventually would.
 *
 * A fiber may spawn another -- a container re-renders its view once its update
 * lands -- so this drains until nothing is left rather than taking a snapshot.
 */
export function runFibers(fibers: Fiber[]): void {
  while (fibers.length > 0) {
    L.shiftRequired(fibers, 'a spawned fiber')()
  }
}

/**
 * Runs just the fiber at `index`, leaving the rest held.
 *
 * The scheduler round-robins the tasks it holds, so an update fiber really can
 * finish while a view fiber is still drawing; this is how a test picks that
 * order.
 */
export function runFiberAt(fibers: Fiber[], index: number): void {
  const [fiber] = fibers.splice(index, 1)
  fiber()
}
