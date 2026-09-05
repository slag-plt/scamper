import { describe, expect, test, vi } from 'vitest'
import { Scheduler } from '../../src/lpm/scheduler'
import { minorStep } from '../../src/lpm/fiber'
import * as U from '../../src/lpm/util'
import {
  makeTask,
  makeTestFiber,
  MockFiber,
  patchSchedulerYieldForTests,
  QUANTUM_WAIT_MS,
  sleep,
} from '../util'

patchSchedulerYieldForTests()

/**
 * Bug (#515): `cancelTask` splices a task out of the run queue while the
 * scheduler loop is suspended part-way through an iteration over that same
 * queue, and the loop resumes holding a `currTaskIdx` that no longer names the
 * task it stepped. `moveNextTask` -- and, for a fiber that finished on that
 * step, `endCurrFiber` -- then act on whatever shifted into the slot.
 *
 * The window is not a race but a guarantee. `execute` awaits
 * `processStepResult` between `stepTask` and `moveNextTask`, and that await is
 * a microtask boundary; a cancel queued with `queueMicrotask` from inside the
 * step therefore always runs first. That is exactly what a truncated trace
 * does: `TraceCollector`'s step-limit hook defers the cancel out of the `send`
 * that tripped it (see `traceStatement` in src/scamper.ts), and the deferral
 * lands *inside* the iteration rather than between iterations.
 *
 * Both tests drive that ordering explicitly -- cancel from `send`, which the
 * scheduler calls synchronously while emitting a completed statement's value --
 * so neither depends on timing.
 */
describe('a cancel mid-iteration does not disturb the queue (#515)', () => {
  /** A fiber that never finishes, for a bystander task to sit behind. */
  const filler = () => {
    const fiber = new MockFiber()
    fiber.stepImpl = () => minorStep
    return fiber
  }

  /**
   * A one-statement program, so the step that emits its value is also the step
   * that finishes the fiber -- the case where `moveNextTask` goes on to
   * `endCurrFiber` and removes a task by index.
   */
  const finishesOnDisplay = () => makeTestFiber([U.mkDisp([U.mkLit(42)])])

  test('a bystander task is not completed in the cancelled one\'s place', async () => {
    const sched = new Scheduler()
    const cancelledDone = vi.fn()
    const bystanderDone = vi.fn()

    // Queued first, so it is the task the cancel shifts the queue beneath.
    const cancelled = {
      ...makeTask(finishesOnDisplay()),
      onComplete: cancelledDone,
    }
    const bystanderFiber = filler()
    const bystander = {
      ...makeTask(bystanderFiber),
      onComplete: bystanderDone,
    }
    vi.spyOn(cancelled.ch, 'send').mockImplementation(() => {
      queueMicrotask(() => {
        sched.cancelTask(cancelled.id)
      })
    })

    sched.schedule(cancelled)
    sched.schedule(bystander)
    await sleep(QUANTUM_WAIT_MS)
    const stepsAtCancel = bystanderFiber.stepCallCount
    await sleep(QUANTUM_WAIT_MS)

    sched.pauseExecution()

    // Nobody cancelled the bystander and it did not finish, so it must not have
    // been told it had: `endCurrFiber` removed it from the queue and fired its
    // `onComplete` because the cancelled task's old slot now held it.
    expect(bystanderDone).not.toHaveBeenCalled()
    // ...and, having been removed, it stopped running.
    expect(bystanderFiber.stepCallCount).toBeGreaterThan(stepsAtCancel)
    // A cancelled run is reported as cancelled, never completed.
    expect(cancelledDone).not.toHaveBeenCalled()
  })

  test('cancelling the only task does not trip the queue atomicity ICE', async () => {
    const sched = new Scheduler()
    const fatal = vi.fn()
    const done = vi.fn()

    // With nothing else queued the splice empties the queue outright, so
    // `removeTaskFromQueue` finds no last task to move into the stale index's
    // slot and raises its atomicity ICE, which `execute` hands to `onFatal`.
    const only = {
      ...makeTask(finishesOnDisplay()),
      onComplete: done,
      onFatal: fatal,
    }
    vi.spyOn(only.ch, 'send').mockImplementation(() => {
      queueMicrotask(() => {
        sched.cancelTask(only.id)
      })
    })

    sched.schedule(only)
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()

    expect(fatal).not.toHaveBeenCalled()
    expect(done).not.toHaveBeenCalled()
  })
})
