import { afterEach, beforeEach, describe, expect, test } from "vitest"
import { Scheduler } from "../src/scheduler"
import { ICE, ScamperError } from "../src/lpm"
import { mkTraceOutput } from "../src/lpm/trace"
import {
  MockFiber,
  QUANTUM_WAIT_MS,
  makeTask,
  sleep,
  withSuppressedRejections,
} from "./test-utils"

describe("Scheduler", () => {
  describe("execution and output", () => {
    test("steps a scheduled fiber while running", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      sched.schedule(makeTask(fiber))
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(fiber.stepCallCount).toBeGreaterThan(0)
    })

    test("sends fiber.lastResult on a completed disp statement", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      fiber.lastStatement = { tag: "disp" }
      fiber.lastResult = 42
      const task = makeTask(fiber, false)
      sched.schedule(task)
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.log.length).toBeGreaterThan(0)
      expect(task.ch.log.every((v) => v === 42)).toBe(true)
    })

    test("does not send disp output while fiber is mid-block", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      fiber.lastStatement = { tag: "disp" }
      fiber.lastResult = "should not appear"
      fiber.isProcessingBlk = true
      const task = makeTask(fiber, false)
      sched.schedule(task)
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.log).toEqual([])
    })

    test("does not send output for non-disp major steps when not tracing", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      fiber.lastStatement = { tag: "stmtexp" }
      fiber.lastResult = "value"
      const task = makeTask(fiber, false)
      sched.schedule(task)
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.log).toEqual([])
    })

    test("sends trace output for non-disp major steps when tracing", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      fiber.lastStatement = { tag: "stmtexp" }
      fiber.lastResult = "traced"
      const task = makeTask(fiber, true)
      sched.schedule(task)
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.log.length).toBeGreaterThan(0)
      // every entry should be a trace-output wrapping the result
      const expected = mkTraceOutput("traced")
      for (const v of task.ch.log) {
        expect(v).toEqual(expected)
      }
    })

    test("disp output is sent raw even with tracing enabled", async () => {
      // disp output takes precedence over the trace-wrapping branch
      const sched = new Scheduler()
      const fiber = new MockFiber()
      fiber.lastStatement = { tag: "disp" }
      fiber.lastResult = 7
      const task = makeTask(fiber, true)
      sched.schedule(task)
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.log.length).toBeGreaterThan(0)
      expect(task.ch.log.every((v) => v === 7)).toBe(true)
    })

    test("minor steps never produce output, even with tracing", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      // even setting lastStatement to disp shouldn't matter: minor step skips
      // the output branch entirely.
      fiber.lastStatement = { tag: "disp" }
      fiber.lastResult = "ignored"
      fiber.stepImpl = () => Promise.resolve(false)
      const task = makeTask(fiber, true)
      sched.schedule(task)
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(fiber.stepCallCount).toBeGreaterThan(0)
      expect(task.ch.log).toEqual([])
    })
  })

  describe("scheduling contract", () => {
    test("schedule throws when given an already-completed fiber", () => {
      const sched = new Scheduler()
      const done = new MockFiber()
      done.isDone = () => true
      expect(() => {
        sched.schedule(makeTask(done))
      }).toThrow()
    })

    test("schedule accepts a fresh (non-done) fiber", () => {
      const sched = new Scheduler()
      const live = new MockFiber()
      expect(() => {
        sched.schedule(makeTask(live))
      }).not.toThrow()
    })

    test("a fiber that becomes done mid-execution is removed and does not halt siblings", async () => {
      // The scheduler's #tasks invariant says only non-done fibers are in
      // the queue, so #execute must remove a fiber as soon as it completes.
      await withSuppressedRejections(async () => {
        const sched = new Scheduler()
        const doneFiber = new MockFiber()
        doneFiber.stepImpl = () => {
          doneFiber.isDone = () => true
          return Promise.resolve(true)
        }
        const liveFiber = new MockFiber()

        sched.schedule(makeTask(doneFiber))
        sched.schedule(makeTask(liveFiber))
        sched.resumeExecution()
        await sleep(QUANTUM_WAIT_MS)
        sched.pauseExecution()
        await sleep(QUANTUM_WAIT_MS)

        // doneFiber should have been stepped exactly once before removal.
        expect(doneFiber.stepCallCount).toBe(1)
        // liveFiber should continue running normally.
        expect(liveFiber.stepCallCount).toBeGreaterThan(1)
      })
    })
  })

  describe("daemon behavior", () => {
    test("a running scheduler picks up tasks scheduled into an empty queue", async () => {
      // The scheduler is a daemon: an empty queue is just an idle state,
      // not a terminal one. Scheduling a task into a running-but-idle
      // scheduler must wake the loop and start stepping the new task
      // without any further nudging from the caller.
      const sched = new Scheduler()
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)

      const fiber = new MockFiber()
      sched.schedule(makeTask(fiber))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(fiber.stepCallCount).toBeGreaterThan(0)
    })
  })

  describe("multiple tasks", () => {
    test("round-robins through all scheduled fibers", async () => {
      const sched = new Scheduler()
      const f1 = new MockFiber()
      const f2 = new MockFiber()
      const f3 = new MockFiber()
      sched.schedule(makeTask(f1))
      sched.schedule(makeTask(f2))
      sched.schedule(makeTask(f3))
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(f1.stepCallCount).toBeGreaterThan(0)
      expect(f2.stepCallCount).toBeGreaterThan(0)
      expect(f3.stepCallCount).toBeGreaterThan(0)
      // round-robin: at any point step counts should differ by at most 1
      const counts = [f1.stepCallCount, f2.stepCallCount, f3.stepCallCount]
      expect(Math.max(...counts) - Math.min(...counts)).toBeLessThanOrEqual(1)
    })

    test("a fiber scheduled while running is picked up on wrap-around", async () => {
      const sched = new Scheduler()
      const f1 = new MockFiber()
      sched.schedule(makeTask(f1))
      sched.resumeExecution()
      // schedule the second fiber while the loop is already running
      const f2 = new MockFiber()
      sched.schedule(makeTask(f2))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(f1.stepCallCount).toBeGreaterThan(0)
      expect(f2.stepCallCount).toBeGreaterThan(0)
    })
  })

  describe("error handling", () => {
    test("ScamperError is caught and reported via the error channel", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      fiber.stepImpl = () =>
        Promise.reject(new ScamperError("Runtime", "boom"))
      const task = makeTask(fiber)
      sched.schedule(task)
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.errLog.length).toBeGreaterThan(0)
      for (const msg of task.ch.errLog) {
        expect(msg).toContain("boom")
      }
    })

    test("ScamperError on one task does not halt other tasks", async () => {
      const sched = new Scheduler()
      const bad = new MockFiber()
      bad.stepImpl = () =>
        Promise.reject(new ScamperError("Runtime", "bad task"))
      const good = new MockFiber()
      const badTask = makeTask(bad)
      const goodTask = makeTask(good)
      sched.schedule(badTask)
      sched.schedule(goodTask)
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(badTask.ch.errLog.length).toBeGreaterThan(0)
      expect(good.stepCallCount).toBeGreaterThan(0)
      // the good fiber's channel should remain clean
      expect(goodTask.ch.errLog).toEqual([])
    })

    test("ICE (non-ScamperError) is not reported through the error channel", async () => {
      // The scheduler re-throws non-ScamperErrors. Because #execute is invoked
      // via `void`, that surfaces as an unhandled rejection. We swallow it so
      // it doesn't fail the test runner, then assert on the observable effects.
      await withSuppressedRejections(async () => {
        const sched = new Scheduler()
        const fiber = new MockFiber()
        fiber.stepImpl = () => Promise.reject(new ICE("test", "internal"))
        const task = makeTask(fiber)
        sched.schedule(task)
        sched.resumeExecution()
        await sleep(QUANTUM_WAIT_MS)
        sched.pauseExecution()
        // ICE shouldn't be reported as a Scamper error
        expect(task.ch.errLog).toEqual([])
        // and execution should not have continued after the throw
        expect(fiber.stepCallCount).toBe(1)
      })
    })
  })

  describe("pause/resume", () => {
    test("pauseExecution stops further stepping", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      sched.schedule(makeTask(fiber))
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      // wait for the in-flight quantum to drain
      await sleep(QUANTUM_WAIT_MS)
      const stableCount = fiber.stepCallCount
      // any further waiting should yield no additional steps
      await sleep(QUANTUM_WAIT_MS)
      expect(fiber.stepCallCount).toBe(stableCount)
    })

    test("resumeExecution restarts a paused scheduler", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      sched.schedule(makeTask(fiber))
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)
      const pausedCount = fiber.stepCallCount
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(fiber.stepCallCount).toBeGreaterThan(pausedCount)
    })

  })

  describe("init", () => {
    let sched: Scheduler | null = null
    beforeEach(() => {
      sched = null
    })
    afterEach(() => {
      sched?.pauseExecution()
    })

    // init() measures the refresh rate over ~1s of wall time, so we bump the
    // per-test timeout.
    test("returns the scheduler instance", async () => {
      sched = new Scheduler()
      const result = await sched.init()
      expect(result).toBe(sched)
    }, 5000)

    test("starts execution so scheduled tasks make progress", async () => {
      sched = new Scheduler()
      const fiber = new MockFiber()
      sched.schedule(makeTask(fiber))
      await sched.init()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(fiber.stepCallCount).toBeGreaterThan(0)
    }, 5000)
  })
})
