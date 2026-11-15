import { beforeEach, describe, expect, test } from 'vitest'
import { localBackend, setBackend } from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { reductionTrace } from '../harness.js'
import * as Scheme from '../../src/scheme'
import { ICE, LoggingChannel, Value } from '../../src/lpm'
import { Fiber } from '../../src/lpm/fiber'
import { Scheduler } from '../../src/lpm/scheduler'
import { makeTraceStepper } from '../../src/scheme/trace'
import { patchSchedulerYieldForTests, QUANTUM_WAIT_MS, sleep } from '../util'

patchSchedulerYieldForTests()

// https://github.com/slag-plt/scamper/issues/758
//
// #737 made a frame completion a major step, so a trace shows the call
// becoming its value -- but only in `Fiber.stepFrame`. A frame can also
// complete on the blocking-primitive path: `Fiber.resumeWithValue` pushes the
// resolved value, finds the frame finished, and calls `completeCurrentFrame`
// without producing a `StepResult` at all, so the scheduler's `block-on`
// branch never classifies it and the tracer is never offered the step.
//
// Every blocking primitive is a library procedure, so each is reached through
// its contract wrapper: the wrapper's frame is what the resume completes, and
// that completion is exactly the reduction in which the call becomes its
// value in the caller. `(string-length (file->string "data.txt"))` therefore
// traces straight to `12`.
//
// The assertions are whole step sequences, as #737's are: a fix that merely
// adds a step somewhere must still produce exactly these reductions, and in
// particular must not add a second copy of a statement's final value.

/** An output channel whose `nth` send throws, standing in for a renderer that
 * breaks on one particular value. */
class ThrowingChannel extends LoggingChannel {
  constructor(private readonly nth: number) {
    super(true, false)
  }

  override send(v: Value): void {
    super.send(v)
    if (this.totalSends === this.nth) {
      throw new ICE('ThrowingChannel.send', 'the renderer broke')
    }
  }
}

let fs: MockFileSystem

beforeEach(async () => {
  // A fresh in-memory FS per test, as test/libs/file.test.ts uses: these are
  // about the trace, not about Node I/O.
  fs = await MockFileSystem.create()
  setBackend(localBackend(fs))
  await fs.saveFile('data.txt', 'hello world\n')
})

describe('a blocking primitive that completes a frame takes a trace step (#758)', () => {
  test("the issue's own program shows file->string reducing", async () => {
    expect(
      await reductionTrace(
        '(import file)\n(string-length (file->string "data.txt"))',
      ),
    ).toEqual([
      '(string-length (file->string "data.txt"))',
      '(string-length "hello world\\n")',
      '12',
    ])
  })

  test('the blocking call need not be the only argument', async () => {
    expect(
      await reductionTrace(
        '(import file)\n(string-append "<" (file->string "data.txt") ">")',
      ),
    ).toEqual([
      '(string-append "<" (file->string "data.txt") ">")',
      '(string-append "<" "hello world\\n" ">")',
      '"<hello world\\n>"',
    ])
  })

  test('two blocking calls take a step each, not one between them', async () => {
    expect(
      await reductionTrace(
        '(import file)\n(list (file->string "data.txt") (file->string "data.txt"))',
      ),
    ).toEqual([
      '(list (file->string "data.txt") (file->string "data.txt"))',
      '(list "hello world\\n" (file->string "data.txt"))',
      '(list "hello world\\n" "hello world\\n")',
    ])
  })

  test('file->string is not the only such primitive', async () => {
    // file-exists? suspends the same way, and `if` shows its test reducing.
    expect(
      await reductionTrace(
        '(import file)\n(if (file-exists? "data.txt") "yes" "no")',
      ),
    ).toEqual([
      '(if (file-exists? "data.txt") "yes" "no")',
      '(if #t "yes" "no")',
      '"yes"',
    ])
  })

  test('a blocking call standing alone gains no duplicate at the end', async () => {
    // The mirror of #737's own last case, and the property a fix must keep:
    // the frame the resume completes here is the statement's own, and with no
    // frames left there is no state to raise -- so the step must not produce a
    // second copy of the value the statement already ends on.
    expect(
      await reductionTrace('(import file)\n(file->string "data.txt")'),
    ).toEqual(['(file->string "data.txt")', '"hello world\\n"'])
  })

  // The reduction is emitted from the resume's own callback, long after the
  // step that suspended the fiber, so the stop the student pressed meanwhile
  // has to be honoured there -- as the rejection branch beside it already does
  // (#577). Pinned here rather than in test/regressions/cancel-suspended-task.
  // test.ts because only a *traced* run can show it: that file's tasks are
  // untraced, so the emit it guards never happens at all.
  test('a run stopped while the read is in flight gains no trace line (#577)', async () => {
    // Hold the read open, so the stop lands while the fiber is suspended on it
    // -- the student pressing stop during a slow read.
    let release!: () => void
    const held = new Promise<void>((resolve) => {
      release = resolve
    })
    const loadFile = fs.loadFile.bind(fs)
    fs.loadFile = (filename: string) => held.then(() => loadFile(filename))

    const { prog } = await Scheme.compile(
      '(import file)\n(string-length (file->string "data.txt"))',
    )
    if (prog === undefined) {
      throw new Error('the program failed to compile')
    }
    const ch = new LoggingChannel(true, false)
    const sched = new Scheduler()
    const task = {
      id: 'stopped-mid-read',
      fiber: new Fiber(prog, Scheme.mkInitialEnv()),
      out: ch,
      err: ch,
      isTracing: true,
      stepper: makeTraceStepper(),
    }

    sched.schedule(task)
    await sleep(QUANTUM_WAIT_MS)
    // Suspended on the read, with the statement's opening state traced.
    expect(ch.log).toEqual(['(string-length (file->string "data.txt"))'])
    const beforeStop = [...ch.log]

    sched.cancelTask(task.id)
    expect(ch.errLog).toHaveLength(1)
    expect(ch.errLog[0]).toContain('cancelled')

    // The read cannot be called off, so it lands anyway -- onto a run that is
    // over. Without the guard its frame completion is traced here.
    release()
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    expect(ch.log).toEqual(beforeStop)
  })

  // The emit above runs in a detached `.then`, outside the try/catch `execute`
  // wraps every step in, so a renderer that throws there used to settle
  // nothing: no `onComplete`, no `onFatal`, and an IDE left with its Run
  // button stuck. The same throw one step later, from inside the loop, has
  // always reached `onFatal` -- so both positions are checked together, which
  // is what says the two paths now agree.
  test.each([
    ['the resume emits the reduction', 2],
    ['the loop emits the statement value', 3],
  ])('a renderer that throws while %s is fatal, not fatal-silent', async (
    _where,
    failingSend,
  ) => {
    const { prog } = await Scheme.compile(
      '(import file)\n(string-length (file->string "data.txt"))',
    )
    if (prog === undefined) {
      throw new Error('the program failed to compile')
    }
    const ch = new ThrowingChannel(failingSend)
    const sched = new Scheduler()
    let completed = false
    let fatal: unknown
    sched.schedule({
      id: `renderer-throws-on-${failingSend.toString()}`,
      fiber: new Fiber(prog, Scheme.mkInitialEnv()),
      out: ch,
      err: ch,
      isTracing: true,
      stepper: makeTraceStepper(),
      onComplete: () => {
        completed = true
      },
      onFatal: (e: unknown) => {
        fatal = e
      },
    })
    await sleep(QUANTUM_WAIT_MS)
    sched.pauseExecution()
    await sleep(QUANTUM_WAIT_MS)

    expect(fatal).toBeInstanceOf(ICE)
    expect(completed).toBe(false)
  })

  test('a blocking call inside a define traces the same way', async () => {
    expect(
      await reductionTrace(
        '(import file)\n(define n (string-length (file->string "data.txt")))',
      ),
    ).toEqual([
      '(string-length (file->string "data.txt"))',
      '(string-length "hello world\\n")',
      '12',
    ])
  })
})
