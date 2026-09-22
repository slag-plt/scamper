import { describe, expect, test, vi } from 'vitest'
import { Scheduler } from '../../src/lpm/scheduler'
import { SuspendSignal, Value } from '../../src/lpm'
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
 * Bug (#577): #534 made a cancel stick for a task suspended on an async action
 * -- the suspension is marked cancelled and `resumeOrComplete` declines to
 * requeue it -- but the action itself cannot be called off. Work already in
 * flight ran all the way to that chokepoint, reporting into the cancelled run's
 * channels on the way, so a student pressing stop during an import saw
 * "Evaluation cancelled" followed by the import's failure message or the
 * imported file's compile diagnostics. The run really was stopped; the extra
 * lines were noise from an action nobody was waiting on any more.
 *
 * Every test below settles the pending action *after* the cancel, explicitly,
 * so there is no race to lose: the cancel is a synchronous call that has
 * already returned by the time the promise is settled.
 */
describe('a cancelled async action reports nothing (#577)', () => {
  /** A task whose only statement is `(import "mod.scm")`, then prints. */
  function importingTask(): TestTask {
    return makeTask(
      makeTestFiber([
        U.mkImport('mod.scm', 'file'),
        U.mkDisp([U.mkLit('after')]),
      ]),
    )
  }

  /**
   * Runs `body` with the file system mocked so `mod.scm` exists and its load is
   * the caller's to settle -- which is what holds the task suspended across the
   * cancel.
   */
  async function withPendingLoad(
    body: (load: {
      resolve: (src: string) => void
      reject: (e: unknown) => void
    }) => Promise<void>,
  ): Promise<void> {
    let resolve!: (src: string) => void
    let reject!: (e: unknown) => void
    const load = new Promise<string>((res, rej) => {
      resolve = res
      reject = rej
    })
    const getFS = vi.spyOn(fs, 'getFS').mockReturnValue({
      fileExists: () => Promise.resolve(true),
      loadFile: () => load,
    } as unknown as ReturnType<typeof fs.getFS>)
    try {
      await body({ resolve, reject })
    } finally {
      getFS.mockRestore()
    }
  }

  test('an import whose load fails after the cancel says nothing more', async () => {
    await withPendingLoad(async (load) => {
      const sched = new Scheduler()
      const task = importingTask()
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)

      sched.cancelTask(task.id)
      expect(task.ch.errLog).toHaveLength(1)
      expect(task.ch.errLog[0]).toContain('cancelled')

      // The read fails once the student has already stopped the run.
      load.reject(new Error('gone'))
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      // 'Evaluation cancelled' is the whole of what the student is told.
      expect(task.ch.errLog).toHaveLength(1)
      expect(task.ch.log).toEqual([])
    })
  })

  test('an import of a file that does not compile says nothing after the cancel', async () => {
    await withPendingLoad(async (load) => {
      const sched = new Scheduler()
      const task = importingTask()
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)

      sched.cancelTask(task.id)
      expect(task.ch.errLog).toHaveLength(1)

      // The load succeeds after the cancel, but the file is broken: its
      // diagnostics belong to a run that is over.
      load.resolve('(define x (lambda (n) (* 2 n)')
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(task.ch.errLog).toHaveLength(1)
      expect(task.ch.errLog[0]).toContain('cancelled')
    })
  })

  test('an import cancelled before its module is scheduled never runs it', async () => {
    await withPendingLoad(async (load) => {
      const sched = new Scheduler()
      const task = importingTask()
      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)

      sched.cancelTask(task.id)

      // A module the importer had not yet scheduled must not be started at
      // all: it runs on the importer's error channel, so anything it raises
      // would land in the stopped run. (#578 is the other half: a module
      // already scheduled when the cancel lands.)
      load.resolve('(error "the module ran")')
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      expect(task.ch.errLog).toHaveLength(1)
      expect(task.ch.errLog[0]).toContain('cancelled')
      expect(task.ch.log).toEqual([])
    })
  })

  test('a blocking primitive whose promise rejects after the cancel says nothing more', async () => {
    const sched = new Scheduler()
    let rejectAction!: (e: unknown) => void
    const action = new Promise<Value>((_res, rej) => {
      rejectAction = rej
    })
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkVar('+'),
        U.mkLit(1),
        U.mkVar('block'),
        U.mkAp(0),
        U.mkAp(2),
      ]),
    ])
    fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel([
      'block',
      () => {
        throw new SuspendSignal(() => action)
      },
    ])
    const task = makeTask(fiber)

    sched.schedule(task)
    await sleep(QUANTUM_WAIT_MS)
    expect(task.ch.log).toEqual([])

    sched.cancelTask(task.id)
    expect(task.ch.errLog).toHaveLength(1)

    // The fetch or read the primitive was waiting on fails after the stop.
    rejectAction(new Error('the fetch failed'))
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    expect(task.ch.errLog).toHaveLength(1)
    expect(task.ch.errLog[0]).toContain('cancelled')
    expect(task.ch.log).toEqual([])
  })
})
