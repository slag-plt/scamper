import { describe, expect, test, vi } from 'vitest'
import { Scheduler, SchedulerId } from '../../src/lpm/scheduler'
import * as U from '../../src/lpm/util'
import * as fs from '../../src/fs'
import {
  makeNeverCompletingFiber,
  makeTask,
  makeTestFiber,
  patchSchedulerYieldForTests,
  QUANTUM_WAIT_MS,
  sleep,
  TestTask,
} from '../util'

patchSchedulerYieldForTests()

/**
 * Bug: an import's existence probe (`fileExists`) is awaited with the task
 * still on the run queue -- it is only pulled off once the probe answers. A
 * stop landing in that window dequeues the task and tells the student the run
 * was cancelled, and then the probe answers and `suspendTask` files a *fresh*
 * suspension marked `cancelled: false`. The cancel is erased: the module is
 * loaded, compiled and run, and when it finishes the importer is put back on
 * the run queue and carries on past the statement the student stopped on.
 *
 * Wider than the three cancellation bugs before it -- #534 (a cancel must
 * stick for a suspended task), #577 (a cancelled run's in-flight action must
 * report nothing), #578 (a cancel must reach the module fiber) -- because it
 * defeats all of them at once: the mark they each read is overwritten before
 * they get to it, and the run itself comes back to life rather than merely
 * emitting noise.
 *
 * The probe is held open by the test, so the cancel lands in that window every
 * time: it is a synchronous call that has already returned by the time the
 * probe is answered.
 */
describe('a cancel during an import probe is not erased', () => {
  interface Deferred<T> {
    promise: Promise<T>
    resolve: (v: T) => void
    reject: (e: unknown) => void
  }

  function deferred<T>(): Deferred<T> {
    let resolve!: (v: T) => void
    let reject!: (e: unknown) => void
    const promise = new Promise<T>((res, rej) => {
      resolve = res
      reject = rej
    })
    return { promise, resolve, reject }
  }

  interface HeldImport {
    /** Settles once `fileExists` has been called and its answer is pending. */
    probeStarted: Promise<void>
    /** The probe's answer, the test's to give. */
    exists: Deferred<boolean>
    /** The file's contents, the test's to give. */
    load: Deferred<string>
    /** How many times the file was read: never for a stopped run, and never
     * twice for a live one. */
    loadCount: () => number
  }

  /**
   * Runs `body` with the file system mocked so an import's *existence probe*
   * hangs until the test answers it -- the window this bug lives in. Waiting on
   * `probeStarted` rather than sleeping makes the ordering straight-line code:
   * the cancel cannot land early, and cannot land late.
   */
  async function withHeldProbe(
    body: (held: HeldImport) => Promise<void>,
  ): Promise<void> {
    // Hand-rolled rather than a Deferred: its value is the *fact* that the
    // probe started, which `Deferred<void>` cannot say in this codebase's
    // lint settings.
    let probeStarted!: () => void
    const started = new Promise<void>((res) => {
      probeStarted = res
    })
    const exists = deferred<boolean>()
    const load = deferred<string>()
    let loads = 0
    const getFS = vi.spyOn(fs, 'getFS').mockReturnValue({
      fileExists: () => {
        probeStarted()
        return exists.promise
      },
      loadFile: () => {
        loads++
        return load.promise
      },
    } as unknown as ReturnType<typeof fs.getFS>)
    try {
      await body({
        probeStarted: started,
        exists,
        load,
        loadCount: () => loads,
      })
    } finally {
      getFS.mockRestore()
    }
  }

  /** A task whose only statement is `(import "mod.scm")`, then prints. */
  function importingTask(): TestTask {
    return makeTask(
      makeTestFiber([
        U.mkImport('mod.scm', 'file'),
        U.mkDisp([U.mkLit('after')]),
      ]),
    )
  }

  /** Lets every pending settle path run, then stops the scheduler for good. */
  async function settle(sched: Scheduler): Promise<void> {
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)
  }

  test('a run stopped during the probe neither loads the module nor resumes', async () => {
    await withHeldProbe(async (held) => {
      const sched = new Scheduler()
      const completed = vi.fn()
      const task = { ...importingTask(), onComplete: completed }

      sched.schedule(task)
      await held.probeStarted

      sched.cancelTask(task.id)
      expect(task.ch.errLog).toHaveLength(1)
      expect(task.ch.errLog[0]).toContain('cancelled')

      // The probe answers after the stop, as a real one does when the student
      // presses stop while the file system is still being asked.
      held.exists.resolve(true)
      held.load.resolve('(error "the module ran")')
      await settle(sched)

      // The importer never came back to life: the statement after the import
      // did not run, and a cancelled run is never reported as completed.
      expect(task.ch.log).toEqual([])
      expect(completed).not.toHaveBeenCalled()
      // And 'Evaluation cancelled' is the whole of what the student is told --
      // the module was never loaded, let alone run.
      expect(task.ch.errLog).toEqual([
        expect.stringContaining('cancelled') as unknown as string,
      ])
    })
  })

  test('a probe that answers "missing" after the stop says nothing more', async () => {
    await withHeldProbe(async (held) => {
      const sched = new Scheduler()
      const task = importingTask()

      sched.schedule(task)
      await held.probeStarted

      sched.cancelTask(task.id)
      held.exists.resolve(false)
      await settle(sched)

      expect(task.ch.errLog).toEqual([
        expect.stringContaining('cancelled') as unknown as string,
      ])
      expect(task.ch.log).toEqual([])
    })
  })

  test('a probe that fails after the stop says nothing more', async () => {
    await withHeldProbe(async (held) => {
      const sched = new Scheduler()
      const task = importingTask()

      sched.schedule(task)
      await held.probeStarted

      sched.cancelTask(task.id)
      held.exists.reject(new Error('the file system is gone'))
      await settle(sched)

      expect(task.ch.errLog).toEqual([
        expect.stringContaining('cancelled') as unknown as string,
      ])
      expect(task.ch.log).toEqual([])
    })
  })

  test('a run stopped during the probe never reads the file, and leaves no suspension behind', async () => {
    await withHeldProbe(async (held) => {
      const sched = new Scheduler()
      const task = importingTask()

      sched.schedule(task)
      await held.probeStarted

      sched.cancelTask(task.id)
      // The file exists, but nobody is waiting for it any more.
      held.exists.resolve(true)
      await settle(sched)

      expect(held.loadCount()).toBe(0)
      // One entry per in-flight action, cleared exactly once. An import called
      // off before it began has no action in flight, so a suspension filed for
      // it is one nothing will ever consume -- holding the run's fiber and
      // channels for the life of the page.
      const { suspensions } = sched as unknown as {
        suspensions: Map<SchedulerId, unknown>
      }
      expect(suspensions.size).toBe(0)
    })
  })

  test('stopping one run leaves an import another is mid-way through alone', async () => {
    await withHeldProbe(async (held) => {
      const sched = new Scheduler()
      const completed = vi.fn()
      const importer = { ...importingTask(), onComplete: completed }
      const other = makeTask(makeNeverCompletingFiber())

      sched.schedule(importer)
      sched.schedule(other)
      await held.probeStarted

      // A stop naming a *different* run still pauses and restarts the execute
      // loop, and the importer is on the queue for it to find: left there
      // across its probe, it is stepped a second time and issues the whole
      // import again -- two loads, two suspensions where one is overwritten,
      // and an importer that never comes back.
      sched.cancelTask(other.id)
      // Long enough for the restarted loop to take a turn at the queue, which
      // is the point: the probe stays pending across it, so the importer is
      // there to be stepped again.
      await sleep(QUANTUM_WAIT_MS)

      held.exists.resolve(true)
      held.load.resolve('(define x 1)')
      await settle(sched)

      expect(held.loadCount()).toBe(1)
      expect(importer.ch.log).toEqual(['after'])
      expect(importer.ch.errLog).toEqual([])
      expect(completed).toHaveBeenCalledTimes(1)
    })
  })

  test('an import nobody stopped still runs and finishes', async () => {
    await withHeldProbe(async (held) => {
      const sched = new Scheduler()
      const completed = vi.fn()
      const task = { ...importingTask(), onComplete: completed }

      sched.schedule(task)
      await held.probeStarted

      held.exists.resolve(true)
      held.load.resolve('(define x 1)')
      await settle(sched)

      expect(task.ch.errLog).toEqual([])
      expect(task.ch.log).toEqual(['after'])
      expect(completed).toHaveBeenCalled()
    })
  })
})
