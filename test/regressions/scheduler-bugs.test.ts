/**
 * Regression tests for known bugs in `src/scheduler.ts`. Each `describe`
 * block documents one bug and asserts the *desired* (post-fix) behavior, so
 * these tests are expected to fail on the current implementation and should
 * turn green as each underlying bug is fixed.
 */
import { describe, expect, test } from "vitest"
import { Scheduler } from "../../src/scheduler"
import { ICE, ScamperError } from "../../src/lpm"
import {
  FakeFiber,
  QUANTUM_WAIT_MS,
  makeTask,
  sleep,
  withSuppressedRejections,
} from "../scheduler-utils"

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
    const fiber = new FakeFiber()

    // Detect overlapping step() calls via an in-flight counter that lives
    // inside stepImpl (which is invoked from FakeFiber.step *after* the
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
    const f1 = new FakeFiber()
    sched.schedule(makeTask(f1))
    sched.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)
    const baseline = f1.stepCallCount
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    // Now start fresh with a brand-new scheduler, but call resume twice.
    const sched2 = new Scheduler()
    const f2 = new FakeFiber()
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
 * Bug: When `Fiber.step()` is called on a fiber that has run past its last
 * statement, it throws an `ICE`. The scheduler's catch block only handles
 * `ScamperError` and re-throws everything else, which (because `#execute`
 * is invoked via `void`) surfaces as an unhandled rejection and tears down
 * the entire loop -- including all other fibers in the queue. The scheduler
 * should instead treat a completed fiber as a no-op (or remove it) and
 * keep stepping its siblings.
 */
describe("a completed fiber does not halt the scheduler", () => {
  test("a fiber whose step() throws ICE does not stop other fibers", async () => {
    await withSuppressedRejections(async () => {
      const sched = new Scheduler()

      // Mimic the real Fiber's "no statements remain" failure mode.
      const doneFiber = new FakeFiber()
      doneFiber.stepImpl = async () => {
        throw new ICE("Fiber.step", "Attempted to step but no statements remain!")
      }
      const liveFiber = new FakeFiber()

      sched.schedule(makeTask(doneFiber))
      sched.schedule(makeTask(liveFiber))
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      // The live fiber should still have made meaningful progress despite
      // its sibling crashing out.
      expect(liveFiber.stepCallCount).toBeGreaterThan(1)
    })
  })
})

/**
 * Bug: The scheduler reads `fiber.lastStatement.tag` *outside* its
 * try/catch. `Fiber.lastStatement` is a getter that throws an `ICE` when
 * no statement has yet been completed (i.e., `currStmtIdx === 0`). Any
 * major step that occurs before the first statement completes therefore
 * crashes the scheduler with an unhandled rejection. Accessing
 * `lastStatement` should either be guarded or moved inside a try/catch.
 */
describe("lastStatement throwing does not halt the scheduler", () => {
  test("a fiber whose lastStatement getter throws does not stop other fibers", async () => {
    await withSuppressedRejections(async () => {
      const sched = new Scheduler()

      // A fiber that reports a major step but whose `lastStatement` getter
      // throws. We install the getter directly on the instance (replacing
      // the field) because subclass getters get shadowed by parent field
      // initializers.
      const bad = new FakeFiber()
      Object.defineProperty(bad, "lastStatement", {
        configurable: true,
        get(): { tag: string } {
          throw new ICE(
            "Fiber.lastStatement",
            "Attempted to get the last completed statement in fiber when none exist at index -1",
          )
        },
      })
      const liveFiber = new FakeFiber()

      sched.schedule(makeTask(bad))
      sched.schedule(makeTask(liveFiber))
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(liveFiber.stepCallCount).toBeGreaterThan(1)
    })
  })
})

/**
 * Bug: When `#execute()` finds an empty task queue, it `return`s outright.
 * No subsequent `schedule()` will wake it back up -- the caller has to
 * explicitly invoke `resumeExecution()` again. In practice this means the
 * very first program submitted to a freshly-init'd scheduler that has
 * since gone idle is silently dropped on the floor. `schedule()` should
 * be sufficient to kick processing back into motion.
 */
describe("scheduling into an idle scheduler resumes processing", () => {
  test("a task scheduled after the queue goes empty is still picked up", async () => {
    const sched = new Scheduler()
    // resumeExecution into an empty queue -> #execute returns immediately.
    sched.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)

    // Now add work. The scheduler should kick the loop back into life on
    // its own, without requiring a manual resumeExecution() from the caller.
    const fiber = new FakeFiber()
    sched.schedule(makeTask(fiber))
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    expect(fiber.stepCallCount).toBeGreaterThan(0)
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
    const fiber = new FakeFiber()
    sched.schedule(makeTask(fiber))
    sched.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)

    const beforePause = fiber.stepCallCount
    sched.pauseExecution()
    // Wait less than a full quantum (~17ms) but long enough for several
    // FakeFiber.step iterations (~1ms each) to slip through if the inner
    // loop does NOT honor the pause flag promptly.
    await sleep(10)
    const afterPause = fiber.stepCallCount

    // At most one step may be in flight when pause is requested. Currently
    // the inner loop ignores the pause flag until the quantum expires,
    // sneaking in many more.
    expect(afterPause - beforePause).toBeLessThanOrEqual(1)
  })
})

/**
 * Bug: When a fiber's `step()` throws a `ScamperError`, the scheduler
 * reports the error, increments `#currTaskIdx`, and `continue`s. If that
 * fiber is the only one in the queue (or simply keeps throwing), it gets
 * re-stepped on the very next iteration of the inner loop -- producing
 * dozens of identical error reports per quantum. A persistently failing
 * fiber should be reported once and then either removed or skipped.
 */
describe("a persistently failing fiber is not stepped repeatedly", () => {
  test("a fiber that always errors does not flood the error log within a single quantum", async () => {
    const sched = new Scheduler()
    const fiber = new FakeFiber()
    fiber.stepImpl = async () => {
      throw new ScamperError("Runtime", "always fails")
    }
    const task = makeTask(fiber)
    sched.schedule(task)
    sched.resumeExecution()
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    // Ideally the failing fiber is removed (or otherwise skipped) after
    // the first error rather than being re-stepped continuously. Currently
    // the scheduler re-steps it on every iteration of the inner loop.
    expect(task.ch.errLog.length).toBeLessThanOrEqual(5)
  })
})
