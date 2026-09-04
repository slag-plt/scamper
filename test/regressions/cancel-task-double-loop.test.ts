import { describe, expect, test, vi } from 'vitest'
import { Scheduler } from '../../src/lpm/scheduler'
import {
  makeTask,
  MockFiber,
  patchSchedulerYieldForTests,
  QUANTUM_WAIT_MS,
  sleep,
} from '../util'

patchSchedulerYieldForTests()

/**
 * Regression for #510: `cancelTask` brackets its work in
 * `pauseExecution()`/`resumeExecution()`, and resume installs a *fresh*
 * `AbortController` before starting a new `execute()`. The loop that was
 * aborted is suspended at an `await`, and when it wakes it asks
 * `this.controller` -- now the new one -- whether it was paused, hears "no",
 * and carries on. Both loops then step the same queue.
 *
 * The damage is to the event loop rather than to the queue: each loop runs a
 * whole time quantum before yielding, so with N of them the page is serviced
 * once every N quanta. Measured here with an unrelated `setTimeout(0)` chain:
 * 25 turns per 300ms with one loop, 4 with six, the longest gap growing from
 * 18ms to 101ms. That is not something to assert on a shared runner, so this
 * test counts the loops themselves -- `execute()` calls that have not returned.
 *
 * N.B. the sibling task matters: cancelling the *only* task empties the queue,
 * and the empty-queue exit clears `isRunning`, which retires the old loop by
 * accident. The leak needs something left to run -- which, since #369, is the
 * ordinary case, as a truncated trace cancels its own task while the student's
 * program is still going.
 *
 * See also `scheduler-bugs.test.ts`, which guards the same one-loop invariant
 * against a second `resumeExecution()`.
 */
describe('cancelTask does not leave a second execute() loop (#510)', () => {
  test('cancelling one of two tasks leaves exactly one loop running', async () => {
    const sched = new Scheduler()
    // `execute` is private and detached (`void this.execute()`), so the only
    // handle on a loop is the promise it returns: one that has not settled is a
    // loop still stepping the queue.
    const realExecute = (
      Scheduler.prototype as unknown as {
        execute: (...args: unknown[]) => Promise<void>
      }
    ).execute
    let started = 0
    let finished = 0
    vi.spyOn(
      sched as unknown as { execute: (...args: unknown[]) => Promise<void> },
      'execute',
    ).mockImplementation((...args: unknown[]) => {
      started++
      const loop = realExecute.apply(sched, args)
      void loop.then(
        () => finished++,
        () => finished++,
      )
      return loop
    })

    const cancelled = makeTask(new MockFiber())
    const sibling = makeTask(new MockFiber())
    sched.schedule(cancelled)
    sched.schedule(sibling)
    await sleep(QUANTUM_WAIT_MS)

    sched.cancelTask(cancelled.id)
    // Long enough for the aborted loop to wake from its yield and notice.
    await sleep(QUANTUM_WAIT_MS)

    expect(started - finished).toBe(1)

    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)
  })
})
