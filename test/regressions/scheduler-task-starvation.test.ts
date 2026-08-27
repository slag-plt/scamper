import { describe, expect, test } from 'vitest'
import { Scheduler, SchedulerTask } from '../../src/lpm/scheduler'
import { minorStep } from '../../src/lpm/fiber'
import {
  makeQueryTask,
  makeTask,
  MockFiber,
  patchSchedulerYieldForTests,
  QUANTUM_WAIT_MS,
  sleep,
} from '../util'

patchSchedulerYieldForTests()

/**
 * Bug (#415): the run queue is round-robin, but a display task's *minor* steps
 * advanced the cursor twice -- once in `processStepResult`, once again in the
 * caller's `moveNextTask`. Minor steps are almost every step a program takes,
 * so whatever sat behind a display task was stepped over on every pass and
 * never ran at all.
 *
 * On a reading page that is a widget which renders everything it renders and
 * then never signals completion, so `runEmbeds` waits on it forever and every
 * widget below it stays blank.
 */
describe('every queued task is stepped (#415)', () => {
  /**
   * Queues one never-ending task per entry in `make` and runs them for a
   * quantum.
   * @returns each fiber's step count, in the order given.
   */
  const stepsTaken = async (
    make: ((fiber: MockFiber) => SchedulerTask)[],
  ): Promise<number[]> => {
    const sched = new Scheduler()
    const fibers = make.map(() => {
      const fiber = new MockFiber()
      // A minor step is the ordinary case -- the one that double-advanced.
      fiber.stepImpl = () => minorStep
      return fiber
    })
    make.forEach((mk, i) => {
      sched.schedule(mk(fibers[i]))
    })
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    return fibers.map((f) => f.stepCallCount)
  }

  test('a display task does not starve the display task behind it', async () => {
    const counts = await stepsTaken([(f) => makeTask(f), (f) => makeTask(f)])
    counts.forEach((c) => {
      expect(c).toBeGreaterThan(0)
    })
  })

  test('a display task does not starve a spawned callback behind it', async () => {
    // The shape a reading page makes: a widget's program, and the fibers its
    // handlers spawn -- query tasks, having no output of their own.
    const counts = await stepsTaken([
      (f) => makeTask(f),
      (f) => makeQueryTask(f),
    ])
    counts.forEach((c) => {
      expect(c).toBeGreaterThan(0)
    })
  })

  test('three display tasks all make progress', async () => {
    const counts = await stepsTaken([
      (f) => makeTask(f),
      (f) => makeTask(f),
      (f) => makeTask(f),
    ])
    counts.forEach((c) => {
      expect(c).toBeGreaterThan(0)
    })
  })
})
