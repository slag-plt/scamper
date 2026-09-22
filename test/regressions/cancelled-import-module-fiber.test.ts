import { describe, expect, test, vi } from 'vitest'
import { Scheduler, SchedulerId, SchedulerTask } from '../../src/lpm/scheduler'
import * as U from '../../src/lpm/util'
import * as fs from '../../src/fs'
import {
  makeTask,
  makeTestFiber,
  patchSchedulerYieldForTests,
  QUANTUM_WAIT_MS,
  sleep,
  TestTask,
} from '../util'

patchSchedulerYieldForTests()

/**
 * Bug (#578): an import runs the imported file as a *second* scheduler task,
 * with an id of its own that nobody outside the scheduler ever learns. A stop
 * names the run's id, so once the module task was scheduled the cancel reached
 * the importer and not the module, which kept running into the stopped run's
 * error channel.
 *
 * #577 closed the other half -- a cancel landing before the load resolves --
 * so every test here settles the load *first* and cancels once the module is
 * on the queue.
 *
 * N.B. a module task is a QueryTask: it has no output channel, so what the
 * imported file *prints* is dropped and only what it *raises* is observable.
 * The tests assert both.
 */
describe('a cancel reaches the module fiber an import spawned (#578)', () => {
  /** A task whose only statement is `(import "mod.scm")`, then prints. */
  function importingTask(): TestTask {
    return makeTask(
      makeTestFiber([
        U.mkImport('mod.scm', 'file'),
        U.mkDisp([U.mkLit('after')]),
      ]),
    )
  }

  interface PendingLoad {
    promise: Promise<string>
    resolve: (src: string) => void
    reject: (e: unknown) => void
  }

  function pendingLoad(): PendingLoad {
    let resolve!: (src: string) => void
    let reject!: (e: unknown) => void
    const promise = new Promise<string>((res, rej) => {
      resolve = res
      reject = rej
    })
    return { promise, resolve, reject }
  }

  /**
   * Runs `body` with the file system mocked so every file exists and each load
   * is the caller's to settle, by name. Nothing resolves on its own, so the
   * orderings below are straight-line code rather than races.
   */
  async function withPendingLoads(
    body: (loadOf: (filename: string) => PendingLoad) => Promise<void>,
  ): Promise<void> {
    const loads = new Map<string, PendingLoad>()
    const loadOf = (filename: string): PendingLoad => {
      const load = loads.get(filename) ?? pendingLoad()
      loads.set(filename, load)
      return load
    }
    const getFS = vi.spyOn(fs, 'getFS').mockReturnValue({
      fileExists: () => Promise.resolve(true),
      loadFile: (filename: string) => loadOf(filename).promise,
    } as unknown as ReturnType<typeof fs.getFS>)
    try {
      await body(loadOf)
    } finally {
      getFS.mockRestore()
    }
  }

  /**
   * Stops `runId` the instant the module fiber for `filename` reaches the run
   * queue -- the window this bug lives in. Driven from `schedule` itself, which
   * the scheduler calls synchronously, so the cancel lands in that window every
   * time rather than when the timing happens to suit.
   */
  function cancelWhenScheduled(
    sched: Scheduler,
    filename: string,
    runId: SchedulerId,
  ): void {
    const schedule = sched.schedule.bind(sched)
    let fired = false
    vi.spyOn(sched, 'schedule').mockImplementation((task: SchedulerTask) => {
      schedule(task)
      if (!fired && task.fiber.modName === filename) {
        fired = true
        sched.cancelTask(runId)
      }
    })
  }

  /** Lets every pending settle path run, then stops the scheduler for good. */
  async function settle(sched: Scheduler): Promise<void> {
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)
  }

  test('a module already on the queue stops when the importer is cancelled', async () => {
    await withPendingLoads(async (loadOf) => {
      const sched = new Scheduler()
      const task = importingTask()
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)

      cancelWhenScheduled(sched, 'mod.scm', task.id)
      loadOf('mod.scm').resolve('(display "printed") (error "the module ran")')
      await settle(sched)

      // 'Evaluation cancelled' is the whole of what the student is told.
      expect(task.ch.errLog).toEqual([
        expect.stringContaining('cancelled') as unknown as string,
      ])
      expect(task.ch.log).toEqual([])
    })
  })

  test('a module suspended on an import of its own stops too', async () => {
    await withPendingLoads(async (loadOf) => {
      const sched = new Scheduler()
      const task = importingTask()
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)

      // The module runs far enough to suspend on its own import, which holds it
      // there while the student presses stop.
      loadOf('mod.scm').resolve('(import "inner.scm")')
      await sleep(QUANTUM_WAIT_MS)

      sched.cancelTask(task.id)
      expect(task.ch.errLog).toHaveLength(1)

      loadOf('inner.scm').resolve('(error "the nested module ran")')
      await settle(sched)

      expect(task.ch.errLog).toEqual([
        expect.stringContaining('cancelled') as unknown as string,
      ])
      expect(task.ch.log).toEqual([])
    })
  })

  test('a nested module already on the queue stops too', async () => {
    await withPendingLoads(async (loadOf) => {
      const sched = new Scheduler()
      const task = importingTask()
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)

      loadOf('mod.scm').resolve('(import "inner.scm")')
      await sleep(QUANTUM_WAIT_MS)

      // Two levels down from the run the student stopped: the cancel has to
      // walk the whole chain, which is what a multi-file project looks like.
      cancelWhenScheduled(sched, 'inner.scm', task.id)
      loadOf('inner.scm').resolve('(error "the nested module ran")')
      await settle(sched)

      expect(task.ch.errLog).toEqual([
        expect.stringContaining('cancelled') as unknown as string,
      ])
      expect(task.ch.log).toEqual([])
    })
  })

  test('a cancelled import leaves no suspension behind', async () => {
    await withPendingLoads(async (loadOf) => {
      const sched = new Scheduler()
      const task = importingTask()
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)

      cancelWhenScheduled(sched, 'mod.scm', task.id)
      loadOf('mod.scm').resolve('(error "the module ran")')
      await settle(sched)

      // The importer was suspended on its module's completion, which cancelling
      // that module makes unreachable -- so the cancel is the last chance to
      // consume the entry. A stale one is silent, holding the run's fiber and
      // channels for the life of the page, so it is checked directly.
      const { suspensions } = sched as unknown as {
        suspensions: Map<SchedulerId, unknown>
      }
      expect(suspensions.size).toBe(0)
    })
  })

  test('a module that completes normally still finishes the import', async () => {
    await withPendingLoads(async (loadOf) => {
      const sched = new Scheduler()
      const task = importingTask()
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)

      loadOf('mod.scm').resolve('(define x 1)')
      await settle(sched)

      expect(task.ch.errLog).toEqual([])
      expect(task.ch.log).toEqual(['after'])
    })
  })
})
