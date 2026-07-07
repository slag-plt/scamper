import { afterEach, beforeEach, describe, expect, test, vi } from "vitest"
import { Scheduler } from "../src/scheduler"
import { ICE, Range, ReportError, ScamperError } from "../src/lpm"
import { mkTraceOutput } from "../src/lpm/trace"
import * as U from "../src/lpm/util"
import {
  makeNeverCompletingFiber,
  makeQueryTask,
  makeReportThrowingFiber,
  makeTask,
  makeTestFiber,
  MockFiber,
  patchSchedulerYieldForTests,
  QUANTUM_WAIT_MS,
  runFiberToCompletion,
  sleep,
  trackFiberSteps,
  withSuppressedRejections,
} from "./test-utils"
import { minorStep, traceStep, yieldStep } from "../src/lpm/fiber"

patchSchedulerYieldForTests()

describe("Scheduler", () => {
  afterEach(() => {
    vi.restoreAllMocks()
  })

  describe("execution and output", () => {
    test("sends fiber.lastResult on a completed disp statement", async () => {
      const sched = new Scheduler()
      const fiber = makeTestFiber([U.mkDisp([U.mkLit(42)])])
      const task = makeTask(fiber, false)
      sched.schedule(task)
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
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.log).toEqual([])
    })

    test("does not send output for non-disp major steps when not tracing", async () => {
      const sched = new Scheduler()
      const fiber = makeTestFiber([U.mkStmtExp([U.mkLit("value")])])
      const task = makeTask(fiber, false)
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.log).toEqual([])
    })

    test("sends trace output for non-disp major steps when tracing", async () => {
      const sched = new Scheduler()
      const fiber = makeTestFiber([U.mkStmtExp([U.mkLit("traced")])])
      const task = makeTask(fiber, true)
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.log.length).toBeGreaterThan(0)
      // the stmtexp's final major step should trace the computed result
      expect(task.ch.log.at(-1)).toEqual(mkTraceOutput("traced"))
    })

    test("disp output is sent raw even with tracing enabled", async () => {
      // disp output takes precedence over the trace-wrapping branch
      const sched = new Scheduler()
      const fiber = makeTestFiber([U.mkDisp([U.mkLit(7)])])
      const task = makeTask(fiber, true)
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.log.length).toBeGreaterThan(0)
      // disp sends raw output even when tracing; earlier trace steps may appear too
      expect(task.ch.log).toContain(7)
    })

    test("minor steps never produce output, even with tracing", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      // even setting lastStatement to disp shouldn't matter: minor step skips
      // the output branch entirely.
      fiber.lastStatement = { tag: "disp" }
      fiber.lastResult = "ignored"
      fiber.stepImpl = () => minorStep
      const task = makeTask(fiber, true)
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(fiber.stepCallCount).toBeGreaterThan(0)
      expect(task.ch.log).toEqual([])
    })
  })

  describe("scheduling contract", () => {
    test("schedule throws when given an already-completed fiber", () => {
      const sched = new Scheduler()
      const done = makeTestFiber([U.mkDisp([U.mkLit(1)])])
      runFiberToCompletion(done)
      expect(() => {
        sched.schedule(makeTask(done))
      }).toThrow()
    })

    test("schedule accepts a fresh (non-done) fiber", () => {
      const sched = new Scheduler()
      const live = makeTestFiber([U.mkDisp([U.mkLit(1)])])
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
          return traceStep
        }
        const liveFiber = trackFiberSteps(makeNeverCompletingFiber())

        sched.schedule(makeTask(doneFiber))
        sched.schedule(makeTask(liveFiber))
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
      await sleep(QUANTUM_WAIT_MS)

      const fiber = trackFiberSteps(makeNeverCompletingFiber())
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
      const f1 = trackFiberSteps(makeNeverCompletingFiber())
      const f2 = trackFiberSteps(makeNeverCompletingFiber())
      const f3 = trackFiberSteps(makeNeverCompletingFiber())
      sched.schedule(makeTask(f1))
      sched.schedule(makeTask(f2))
      sched.schedule(makeTask(f3))
      // The first schedule() runs #execute synchronously through an entire
      // time quantum before later schedule() calls add siblings, so total
      // step counts are skewed. Snapshot after warmup, then assert on deltas.
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      const baseline = [f1.stepCallCount, f2.stepCallCount, f3.stepCallCount]
      sched.resumeExecution()
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()

      expect(f1.stepCallCount).toBeGreaterThan(baseline[0])
      expect(f2.stepCallCount).toBeGreaterThan(baseline[1])
      expect(f3.stepCallCount).toBeGreaterThan(baseline[2])
      const delta = [
        f1.stepCallCount - baseline[0],
        f2.stepCallCount - baseline[1],
        f3.stepCallCount - baseline[2],
      ]
      expect(Math.max(...delta) - Math.min(...delta)).toBeLessThanOrEqual(1)
    })

    test("a fiber scheduled while running is picked up on wrap-around", async () => {
      const sched = new Scheduler()
      const f1 = trackFiberSteps(makeNeverCompletingFiber())
      sched.schedule(makeTask(f1))
      // schedule the second fiber while the loop is already running
      const f2 = trackFiberSteps(makeNeverCompletingFiber())
      sched.schedule(makeTask(f2))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(f1.stepCallCount).toBeGreaterThan(0)
      expect(f2.stepCallCount).toBeGreaterThan(0)
    })

    test("runs other fibers while one fiber is yielding, then resumes the yielding fiber", async () => {
      const sched = new Scheduler()
      const yieldCount = 10
      let yieldsRemaining = yieldCount
      let otherStepsWhileYielding = 0

      const yieldingFiber = new MockFiber()
      yieldingFiber.stepImpl = () => {
        if (yieldsRemaining > 0) {
          yieldsRemaining--
          return yieldStep
        }
        return traceStep
      }

      const otherFiber = new MockFiber()
      otherFiber.stepImpl = () => {
        if (yieldsRemaining > 0) {
          otherStepsWhileYielding++
        }
        return traceStep
      }

      // Schedule the non-yielding fiber first: the first schedule() runs a
      // full synchronous quantum before the second is queued, so the other
      // fiber must be first to step while yieldsRemaining > 0.
      sched.schedule(makeTask(otherFiber))
      sched.schedule(makeTask(yieldingFiber))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()

      // round-robin should have stepped the other fiber during the yield window
      expect(otherStepsWhileYielding).toBeGreaterThan(0)
      // the yielding fiber should have exhausted its yields and continued stepping
      expect(yieldsRemaining).toBe(0)
      expect(yieldingFiber.stepCallCount).toBeGreaterThan(yieldCount)
      expect(otherFiber.stepCallCount).toBeGreaterThan(0)
    })
  })

  describe("error handling", () => {
    test("ScamperError is caught and reported via the error channel", async () => {
      const sched = new Scheduler()
      const fiber = new MockFiber()
      fiber.stepImpl = () => {
        throw new ScamperError("Runtime", "boom")
      }
      const task = makeTask(fiber)
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(task.ch.errLog.length).toBeGreaterThan(0)
      for (const msg of task.ch.errLog) {
        expect(msg).toContain("boom")
      }
    })

    test.skip("ScamperError on one task does not halt other tasks", async () => {
      const sched = new Scheduler()
      const bad = new MockFiber()
      bad.stepImpl = () => {
        throw new ScamperError("Runtime", "bad task")
      }
      const good = trackFiberSteps(makeNeverCompletingFiber())
      const badTask = makeTask(bad)
      const goodTask = makeTask(good)
      sched.schedule(badTask)
      sched.schedule(goodTask)
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
        fiber.stepImpl = () => {
          throw new ICE("test", "internal")
        }
        const task = makeTask(fiber)
        sched.schedule(task)
        await sleep(QUANTUM_WAIT_MS)
        sched.pauseExecution()
        // ICE shouldn't be reported as a Scamper error
        expect(task.ch.errLog).toEqual([])
        // and execution should not have continued after the throw
        expect(fiber.stepCallCount).toBe(1)
      })
    })
  })

  describe("query / report handling", () => {
    describe("query scheduling contract", () => {
      test("query throws when given an already-completed fiber", () => {
        const sched = new Scheduler()
        const done = makeTestFiber([U.mkDisp([U.mkLit(1)])])
        runFiberToCompletion(done)
        expect(() => {
          sched.schedule(makeQueryTask(done))
        }).toThrow()
      })

      test("query accepts a fresh (non-done) fiber", () => {
        const sched = new Scheduler()
        const live = makeTestFiber([U.mkDisp([U.mkLit(1)])])
        expect(() => {
          sched.schedule(makeQueryTask(live))
        }).not.toThrow()
      })
    })

    test("delivers ReportError to rep with the reported value and removes the fiber", async () => {
      const sched = new Scheduler()
      const reportedValue = 42
      const reportFiber = makeReportThrowingFiber(reportedValue)
      const sibling = trackFiberSteps(makeNeverCompletingFiber())
      const queryTask = makeQueryTask(reportFiber)

      sched.schedule(queryTask)
      sched.schedule(makeTask(sibling))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(queryTask.err.errors).toHaveLength(1)
      const reported = queryTask.err.errors[0]
      expect(reported).toBeInstanceOf(ReportError)
      expect((reported as ReportError).value).toBe(reportedValue)
      expect(reportFiber.stepCallCount).toBe(1)
      expect(sibling.stepCallCount).toBeGreaterThan(0)
    })

    test("delivers ScamperError to rep and removes the fiber", async () => {
      const sched = new Scheduler()
      const errorFiber = new MockFiber()
      errorFiber.stepImpl = () => {
        throw new ScamperError("Runtime", "query boom")
      }
      const sibling = trackFiberSteps(makeNeverCompletingFiber())
      const queryTask = makeQueryTask(errorFiber)

      sched.schedule(queryTask)
      sched.schedule(makeTask(sibling))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(queryTask.err.errors).toHaveLength(1)
      expect(queryTask.err.errors[0].message).toContain("query boom")
      expect(errorFiber.stepCallCount).toBe(1)
      expect(sibling.stepCallCount).toBeGreaterThan(0)
    })

    test("a QueryTask produces no display output and is removed on completion", async () => {
      const sched = new Scheduler()
      const doneFiber = new MockFiber()
      doneFiber.stepImpl = () => {
        doneFiber.isDone = () => true
        return traceStep
      }
      const sibling = trackFiberSteps(makeNeverCompletingFiber())
      const queryTask = makeQueryTask(doneFiber)

      sched.schedule(queryTask)
      sched.schedule(makeTask(sibling))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(queryTask.err.errors).toEqual([])
      expect(doneFiber.stepCallCount).toBe(1)
      expect(sibling.stepCallCount).toBeGreaterThan(0)
    })

    test("ReportError on a DisplayTask surfaces the friendly reported value and advances the statement", async () => {
      // A `report` reaching a DisplayTask (rather than a QueryTask) is the
      // non-query path. The user-facing string comes from ReportError's own
      // message, which is "Reported value: <value>" (see src/lpm/error.ts), so
      // a literal report shows the value rather than an internal diagnostic.
      const sched = new Scheduler()
      const reportedValue = "displayed report"
      const fiber = makeReportThrowingFiber(reportedValue)
      const advanceStmt = vi.spyOn(fiber, "advanceStmt")
      const task = makeTask(fiber)

      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()

      expect(task.ch.errLog).toHaveLength(1)
      expect(task.ch.errLog[0]).toContain(`Reported value: "${reportedValue}"`)
      expect(advanceStmt).toHaveBeenCalledOnce()
      expect(task.ch.log).toEqual([])
    })

    test("the reported value carries the originating source range", async () => {
      const sched = new Scheduler()
      const range = Range.of(1, 2, 3, 4, 5, 6)
      const reportFiber = makeReportThrowingFiber("ranged", range)
      const queryTask = makeQueryTask(reportFiber)

      sched.schedule(queryTask)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()

      expect(queryTask.err.errors).toHaveLength(1)
      expect((queryTask.err.errors[0] as ReportError).range).toBe(range)
    })

    test("a reporting QueryTask is removed without dropping or duplicating sibling tasks", async () => {
      // #removeCurrFiber swaps the *last* task into the removed slot then pops.
      // With the query task at index 0 and two siblings after it, removing the
      // query relocates the last sibling into slot 0 mid-iteration; both
      // siblings must keep running (none dropped, none double-counted).
      const sched = new Scheduler()
      const reportFiber = makeReportThrowingFiber("sibling check", Range.none)
      const siblingA = trackFiberSteps(makeNeverCompletingFiber())
      const siblingB = trackFiberSteps(makeNeverCompletingFiber())
      const queryTask = makeQueryTask(reportFiber)

      sched.schedule(queryTask)
      sched.schedule(makeTask(siblingA))
      sched.schedule(makeTask(siblingB))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(queryTask.err.errors).toHaveLength(1)
      expect((queryTask.err.errors[0] as ReportError).value).toBe(
        "sibling check",
      )
      expect(reportFiber.stepCallCount).toBe(1)
      expect(siblingA.stepCallCount).toBeGreaterThan(1)
      expect(siblingB.stepCallCount).toBeGreaterThan(1)
    })
  })

  describe("cancelTask", () => {
    test("reports Evaluation cancelled and removes the task from the queue", async () => {
      const sched = new Scheduler()
      const fiber = trackFiberSteps(makeNeverCompletingFiber())
      const task = makeTask(fiber)
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      const stepsBeforeCancel = fiber.stepCallCount
      expect(stepsBeforeCancel).toBeGreaterThan(0)

      sched.resumeExecution()
      sched.cancelTask(task.id)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(task.ch.errLog).toHaveLength(1)
      expect(task.ch.errLog[0]).toContain("Evaluation cancelled")

      const stepsAfterCancel = fiber.stepCallCount
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(fiber.stepCallCount).toBe(stepsAfterCancel)
    })

    test("does not halt sibling tasks", async () => {
      const sched = new Scheduler()
      const cancelled = trackFiberSteps(makeNeverCompletingFiber())
      const sibling = trackFiberSteps(makeNeverCompletingFiber())
      const cancelledTask = makeTask(cancelled)
      sched.schedule(cancelledTask)
      sched.schedule(makeTask(sibling))
      await sleep(QUANTUM_WAIT_MS)

      sched.cancelTask(cancelledTask.id)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(cancelledTask.ch.errLog).toHaveLength(1)
      expect(cancelledTask.ch.errLog[0]).toContain("Evaluation cancelled")

      const cancelledSteps = cancelled.stepCallCount
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(cancelled.stepCallCount).toBe(cancelledSteps)
      expect(sibling.stepCallCount).toBeGreaterThan(0)
    })

    test("cancels a query task via its error channel", async () => {
      const sched = new Scheduler()
      const fiber = trackFiberSteps(makeNeverCompletingFiber())
      const queryTask = makeQueryTask(fiber)
      sched.schedule(queryTask)
      await sleep(QUANTUM_WAIT_MS)

      sched.cancelTask(queryTask.id)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()

      expect(queryTask.err.errors).toHaveLength(1)
      expect(queryTask.err.errors[0].message).toContain("Evaluation cancelled")
    })

    test("does not pause execution when the task id is unknown", async () => {
      const sched = new Scheduler()
      const fiber = trackFiberSteps(makeNeverCompletingFiber())
      sched.schedule(makeTask(fiber))
      await sleep(QUANTUM_WAIT_MS)

      sched.cancelTask(crypto.randomUUID())
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(fiber.stepCallCount).toBeGreaterThan(0)
    })

    test("does not resume execution when the scheduler was already paused", async () => {
      const sched = new Scheduler()
      const fiber = trackFiberSteps(makeNeverCompletingFiber())
      const task = makeTask(fiber)
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      const pausedCount = fiber.stepCallCount
      sched.cancelTask(task.id)
      await sleep(QUANTUM_WAIT_MS)

      expect(task.ch.errLog).toHaveLength(1)
      expect(fiber.stepCallCount).toBe(pausedCount)
    })

    test("yields before stepping fibers in a frame", async () => {
      const sched = new Scheduler()
      const fiber = trackFiberSteps(makeNeverCompletingFiber())
      sched.schedule(makeTask(fiber))
      expect(fiber.stepCallCount).toBe(0)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(fiber.stepCallCount).toBeGreaterThan(0)
    })

    test("calls onComplete when a display task finishes normally", async () => {
      const sched = new Scheduler()
      const fiber = makeTestFiber([U.mkDisp([U.mkLit(1)])])
      const onComplete = vi.fn()
      const task = { ...makeTask(fiber, false), onComplete }
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()

      expect(onComplete).toHaveBeenCalledOnce()
    })

    test("does not call onComplete when a display task is cancelled", async () => {
      const sched = new Scheduler()
      const fiber = trackFiberSteps(makeNeverCompletingFiber())
      const onComplete = vi.fn()
      const task = { ...makeTask(fiber, false), onComplete }
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)

      sched.cancelTask(task.id)
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()

      expect(onComplete).not.toHaveBeenCalled()
    })
  })

  describe("pause/resume", () => {
    test("pauseExecution stops further stepping", async () => {
      const sched = new Scheduler()
      const fiber = trackFiberSteps(makeNeverCompletingFiber())
      sched.schedule(makeTask(fiber))
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
      const fiber = trackFiberSteps(makeNeverCompletingFiber())
      sched.schedule(makeTask(fiber))
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

  describe("construction", () => {
    let sched: Scheduler | null = null
    beforeEach(() => {
      sched = null
    })
    afterEach(() => {
      sched?.pauseExecution()
    })

    test("constructor initializes a runnable scheduler", () => {
      sched = new Scheduler()
      expect(sched).toBeInstanceOf(Scheduler)
    })

    test("starts execution on construction so scheduled tasks make progress", async () => {
      sched = new Scheduler()
      const fiber = trackFiberSteps(makeNeverCompletingFiber())
      sched.schedule(makeTask(fiber))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      expect(fiber.stepCallCount).toBeGreaterThan(0)
    })
  })
})
