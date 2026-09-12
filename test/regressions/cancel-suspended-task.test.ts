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
} from '../util'

patchSchedulerYieldForTests()

/**
 * Bug (#534): a task suspended on an async action -- a blocking primitive
 * (`with-file`, `image-load`, `with-image-from-url`) or a file `import` -- has
 * already been pulled out of the run queue by the step that suspended it, and
 * it is not in `steppingGates` either. `cancelTask` looks only in those two
 * places, so it takes its early return and does nothing: the owner is never
 * told its run was cancelled, and no gate is settled.
 *
 * The suspension is still pending, too. When the action resolves,
 * `resumeOrComplete` puts the "cancelled" task back on the run queue and it
 * carries on stepping -- so pressing stop while a program waits on a fetch or a
 * read appears to do nothing, and the program finishes on its own.
 *
 * Distinct from #515, which was a cancel acting on the *wrong* task because an
 * index went stale; this is a cancel acting on *no* task.
 */
describe('cancelling a task suspended on an async action (#534)', () => {
  /**
   * `(+ 1 (block))`, where `(block)` suspends the fiber on `action` -- the
   * shape every blocking primitive takes. The statement is the program's last,
   * so resuming it runs to completion and is impossible to miss.
   */
  function blockingTask(action: Promise<Value>) {
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
    return makeTask(fiber)
  }

  test('a task suspended in block-on is told it was cancelled', async () => {
    const sched = new Scheduler()
    let resolveAction!: (v: Value) => void
    const action = new Promise<Value>((r) => {
      resolveAction = r
    })
    const task = blockingTask(action)

    sched.schedule(task)
    await sleep(QUANTUM_WAIT_MS)
    // Suspended: out of the run queue, nothing emitted yet.
    expect(task.ch.log).toEqual([])

    sched.cancelTask(task.id)
    sched.pauseExecution()

    // The owner's error channel is how a cancelled run is reported, exactly as
    // it is for a queued task.
    expect(task.ch.errLog).toHaveLength(1)
    expect(task.ch.errLog[0]).toContain('cancelled')

    resolveAction(5)
    await sleep(QUANTUM_WAIT_MS)
  })

  test('a task cancelled while suspended in block-on does not come back', async () => {
    const sched = new Scheduler()
    const completed = vi.fn()
    let resolveAction!: (v: Value) => void
    const action = new Promise<Value>((r) => {
      resolveAction = r
    })
    const task = { ...blockingTask(action), onComplete: completed }

    sched.schedule(task)
    await sleep(QUANTUM_WAIT_MS)
    sched.cancelTask(task.id)

    // The pending action settles after the cancel, as a fetch or a file read
    // does when the student presses stop mid-flight.
    resolveAction(5)
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    // Nothing more may run: no `(+ 1 5)` printed, and a cancelled run is never
    // reported as completed.
    expect(task.ch.log).toEqual([])
    expect(completed).not.toHaveBeenCalled()
  })

  test('a step-mode task cancelled while suspended does not come back', async () => {
    const sched = new Scheduler()
    const completed = vi.fn()
    let resolveAction!: (v: Value) => void
    const action = new Promise<Value>((r) => {
      resolveAction = r
    })
    // A step-mode run *does* have a gate, so `cancelTask` finds it and reports.
    // Only the second half of the defect shows here -- which is exactly why the
    // cancelled mark has to be set whichever branch cancelTask takes.
    const task = {
      ...blockingTask(action),
      stepping: true,
      onComplete: completed,
    }

    sched.schedule(task)
    const settled = sched.resume(task.id, 'all')
    await sleep(QUANTUM_WAIT_MS)
    expect(task.ch.log).toEqual([])

    sched.cancelTask(task.id)
    expect(task.ch.errLog).toHaveLength(1)
    await settled

    resolveAction(5)
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    expect(task.ch.log).toEqual([])
    expect(completed).not.toHaveBeenCalled()
  })

  test('a task cancelled while suspended in import-file does not come back', async () => {
    const sched = new Scheduler()
    const completed = vi.fn()
    let resolveLoad!: (src: string) => void
    const load = new Promise<string>((r) => {
      resolveLoad = r
    })
    // The import's existence probe answers at once, so the task suspends on the
    // *load*, which the test holds open across the cancel.
    const getFS = vi.spyOn(fs, 'getFS').mockReturnValue({
      fileExists: () => Promise.resolve(true),
      loadFile: () => load,
    } as unknown as ReturnType<typeof fs.getFS>)
    try {
      const fiber = makeTestFiber([
        U.mkImport('mod.scm', 'file'),
        U.mkDisp([U.mkLit('after')]),
      ])
      const task = { ...makeTask(fiber), onComplete: completed }

      sched.schedule(task)
      await sleep(QUANTUM_WAIT_MS)
      expect(task.ch.log).toEqual([])

      sched.cancelTask(task.id)
      expect(task.ch.errLog).toHaveLength(1)
      expect(task.ch.errLog[0]).toContain('cancelled')

      resolveLoad('(define x 1)')
      await sleep(QUANTUM_WAIT_MS)
      sched.pauseExecution()
      await sleep(QUANTUM_WAIT_MS)

      // The statement after the import must never run.
      expect(task.ch.log).toEqual([])
      expect(completed).not.toHaveBeenCalled()
    } finally {
      getFS.mockRestore()
    }
  })

  test('a second cancel of a suspended task says nothing more', async () => {
    const sched = new Scheduler()
    let resolveAction!: (v: Value) => void
    const action = new Promise<Value>((r) => {
      resolveAction = r
    })
    const task = blockingTask(action)

    sched.schedule(task)
    await sleep(QUANTUM_WAIT_MS)
    // The entry outlives the first cancel -- only the action's settle path can
    // clear it -- so a second cancel can still find it. A queued task is
    // idempotent for free, the first cancel having dequeued it, and a suspended
    // one must read the same to the student: one 'Evaluation cancelled'.
    sched.cancelTask(task.id)
    sched.cancelTask(task.id)
    sched.pauseExecution()

    expect(task.ch.errLog).toHaveLength(1)

    resolveAction(5)
    await sleep(QUANTUM_WAIT_MS)
  })

  test('a cancel after the suspension settled is not reported', async () => {
    const sched = new Scheduler()
    const completed = vi.fn()
    let resolveAction!: (v: Value) => void
    const action = new Promise<Value>((r) => {
      resolveAction = r
    })
    const task = { ...blockingTask(action), onComplete: completed }

    sched.schedule(task)
    await sleep(QUANTUM_WAIT_MS)
    resolveAction(5)
    await sleep(QUANTUM_WAIT_MS)
    // The run settled and finished, so there is no suspension left to find.
    expect(task.ch.log).toEqual([6])
    expect(completed).toHaveBeenCalled()

    // Pressing stop once a run has finished has always been a no-op; an entry
    // left behind by the settle would make this report against it.
    sched.cancelTask(task.id)
    sched.pauseExecution()

    expect(task.ch.errLog).toEqual([])
  })
})
