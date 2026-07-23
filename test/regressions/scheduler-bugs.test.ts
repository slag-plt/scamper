import { describe, expect, test, vi } from 'vitest'
import { Scheduler } from '../../src/lpm/scheduler'
import {
  makeTask,
  MockFiber,
  patchSchedulerYieldForTests,
  QUANTUM_WAIT_MS,
  sleep,
} from '../util'
import { traceStep } from '../../src/lpm/fiber'

patchSchedulerYieldForTests()

// Regressions for unintended bugs in `src/scheduler.ts`. Each `describe`
// block documents one bug and asserts the *desired* (post-fix) behavior,
// so failing tests double as a to-do list. Tests that no longer fail are
// kept as guards against the bug returning.
//
// N.B., contract / intended-behavior tests live in test/scheduler.test.ts.

/**
 * Bug: `resumeExecution()` unconditionally fires `void this.#execute()`
 * with no guard against an already-running loop. Calling it while a loop
 * is already in flight spawns a second concurrent `#execute()`, which races
 * on `#currTaskIdx` and on each fiber's `step()`. The visible symptoms are
 * (a) the same fiber being stepped concurrently and (b) the overall stepping
 * rate roughly doubling.
 */
describe('no concurrent #execute() loops', () => {
  test('a second resumeExecution() does not double-step the same fiber', async () => {
    const sched = new Scheduler()
    const fiber = new MockFiber()

    // Detect overlapping step() calls via an in-flight counter that lives
    // inside stepImpl (which is invoked from MockFiber.step *after* the
    // synchronous stepCallCount bump).
    let inFlight = 0
    let maxInFlight = 0
    fiber.stepImpl = () => {
      inFlight++
      maxInFlight = Math.max(maxInFlight, inFlight)
      inFlight--
      return traceStep
    }

    sched.schedule(makeTask(fiber))
    // Constructor already started execution
    sched.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    // Allow any in-flight work to drain.
    await sleep(QUANTUM_WAIT_MS)

    // With a single #execute() loop, step calls are strictly sequential.
    expect(maxInFlight).toBe(1)
  })

  test('a second resumeExecution() does not start a second execute() loop', async () => {
    const sched = new Scheduler()
    const fiber = new MockFiber()
    // schedule() starts the one and only execute() loop. The doubled stepping
    // rate this guards against is the visible symptom of a second concurrent
    // loop, so count execute() invocations directly rather than timing steps:
    // a running scheduler must treat further resumeExecution() calls as
    // no-ops, adding zero new loops.
    sched.schedule(makeTask(fiber))
    const executeSpy = vi.spyOn(
      sched as unknown as { execute: () => Promise<void> },
      'execute',
    )

    sched.resumeExecution()
    sched.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    expect(executeSpy).not.toHaveBeenCalled()
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
describe('pauseExecution takes effect promptly', () => {
  test('after pauseExecution, at most one additional step occurs', async () => {
    const sched = new Scheduler()
    const fiber = new MockFiber()
    sched.schedule(makeTask(fiber))
    await sleep(QUANTUM_WAIT_MS)

    const beforePause = fiber.stepCallCount
    sched.pauseExecution()
    // Wait less than a full quantum (~17ms) but long enough for several
    // MockFiber.step iterations to slip through if the inner loop does NOT
    // honor the pause flag promptly.
    await sleep(10)
    const afterPause = fiber.stepCallCount

    // At most one step may be in flight when pause is requested. Currently
    // the inner loop ignores the pause flag until the quantum expires,
    // sneaking in many more.
    expect(afterPause - beforePause).toBeLessThanOrEqual(1)
  })
})
