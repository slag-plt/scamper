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
 * Bug (#415): the run queue removes a finished task by swapping the last one
 * into its slot and popping -- but it returned what `pop()` gave back, which is
 * the task that was *moved*, not the one that finished. Unless the finished
 * task happened to be last, its owner was never told it had finished and some
 * other task was told twice.
 *
 * A run's owner waits on that signal, so an embedded widget rendered everything
 * it renders and then hung; `runEmbeds` awaits each widget before starting the
 * next, so the rest of the reading stayed blank. Which task is last depends on
 * what else is queued at that instant, which is why it came and went with load.
 */
describe('a finished task is the one signalled complete (#415)', () => {
  /** A task that never finishes, to sit behind the one that does. */
  const filler = () => {
    const fiber = new MockFiber()
    fiber.stepImpl = () => minorStep
    return fiber
  }

  test('the first of several tasks reports its own completion', async () => {
    const sched = new Scheduler()
    const finishes = vi.fn()
    const behind = vi.fn()

    // Queued first, so the two behind it are what the swap reaches for.
    sched.schedule({
      ...makeTask(makeTestFiber([U.mkDisp([U.mkLit(42)])])),
      onComplete: finishes,
    })
    sched.schedule({ ...makeTask(filler()), onComplete: behind })
    sched.schedule({ ...makeTask(filler()), onComplete: behind })

    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()

    expect(finishes).toHaveBeenCalledOnce()
    // And nobody else was told it had finished on its behalf.
    expect(behind).not.toHaveBeenCalled()
  })
})
