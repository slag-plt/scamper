import { describe, expect, test } from "vitest"
import { Scheduler } from "../../src/scheduler"
import {
  MockFiber,
  QUANTUM_WAIT_MS,
  makeTask,
  sleep,
} from "../test-utils"

// Regressions for unintended bugs in `src/scheduler.ts`. Each `describe`
// block documents one bug and asserts the *desired* (post-fix) behavior,
// so failing tests double as a to-do list. Tests that no longer fail are
// kept as guards against the bug returning.
//
// N.B., contract / intended-behavior tests live in test/scheduler.test.ts.

/**
 * Bug: `resumeExecution()` (and `init()`) unconditionally fire
 * `void this.#execute()` with no guard against an already-running loop.
 * Calling either method while a loop is already in flight spawns a second
 * concurrent `#execute()`, which races on `#currTaskIdx` and on each
 * fiber's `step()`. The visible symptoms are (a) the same fiber being
 * stepped concurrently and (b) the overall stepping rate roughly doubling.
 */
describe("no concurrent #execute() loops", () => {
  test("a second resumeExecution() does not double-step the same fiber", async () => {
    const sched = new Scheduler()
    const fiber = new MockFiber()

    // Detect overlapping step() calls via an in-flight counter that lives
    // inside stepImpl (which is invoked from MockFiber.step *after* the
    // synchronous stepCallCount bump).
    let inFlight = 0
    let maxInFlight = 0
    fiber.stepImpl = async () => {
      inFlight++
      maxInFlight = Math.max(maxInFlight, inFlight)
      await sleep(5)
      inFlight--
      return true
    }

    sched.schedule(makeTask(fiber))
    sched.resumeExecution()
    // Second resume should be a no-op for an already-running scheduler.
    sched.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    // Allow any in-flight work to drain.
    await sleep(QUANTUM_WAIT_MS)

    // With a single #execute() loop, step calls are strictly sequential.
    expect(maxInFlight).toBe(1)
  })

  test("a second resumeExecution() does not roughly double the stepping rate", async () => {
    const sched = new Scheduler()
    const f1 = new MockFiber()
    sched.schedule(makeTask(f1))
    sched.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)
    const baseline = f1.stepCallCount
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    // Now start fresh with a brand-new scheduler, but call resume twice.
    const sched2 = new Scheduler()
    const f2 = new MockFiber()
    sched2.schedule(makeTask(f2))
    sched2.resumeExecution()
    sched2.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)
    sched2.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    // The double-resume should not produce ~2x stepping. Allow generous
    // slack for timing noise.
    expect(f2.stepCallCount).toBeLessThan(baseline * 1.8)
  })
})

/**
 * Bug: `pauseExecution()` only flips `#isRunning` to false, which is
 * checked at the *outer* while loop. The inner loop continues stepping
 * until the current time quantum (~17ms) expires, so up to a quantum's
 * worth of additional steps run after the caller has requested a pause.
 * Pause should propagate to the inner loop too, so at most one in-flight
 * step completes after the request.
 */
describe("pauseExecution takes effect promptly", () => {
  test("after pauseExecution, at most one additional step occurs", async () => {
    const sched = new Scheduler()
    const fiber = new MockFiber()
    sched.schedule(makeTask(fiber))
    sched.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)

    const beforePause = fiber.stepCallCount
    sched.pauseExecution()
    // Wait less than a full quantum (~17ms) but long enough for several
    // MockFiber.step iterations (~1ms each) to slip through if the inner
    // loop does NOT honor the pause flag promptly.
    await sleep(10)
    const afterPause = fiber.stepCallCount

    // At most one step may be in flight when pause is requested. Currently
    // the inner loop ignores the pause flag until the quantum expires,
    // sneaking in many more.
    expect(afterPause - beforePause).toBeLessThanOrEqual(1)
  })
})
