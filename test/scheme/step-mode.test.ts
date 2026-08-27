import { describe, expect, test } from 'vitest'
import * as Scheme from '../../src/scheme/index.js'
import { Fiber } from '../../src/lpm/fiber.js'
import { Scheduler } from '../../src/lpm/scheduler.js'
import type { StepMode } from '../../src/lpm/scheduler.js'
import { makeTraceStepper } from '../../src/scheme/trace.js'
import { expToString, mkLit } from '../../src/scheme/ast.js'
import * as U from '../../src/lpm/util.js'
import { isStructKind } from '../../src/lpm/util.js'
import { SuspendSignal } from '../../src/lpm/index.js'
import type { Value } from '../../src/lpm/lang.js'
import { makeTestFiber } from '../util.js'
import { required } from '../dom'

// End-to-end tests for the scheduler's step mode: a run pauses ("parks") after
// each user-visible reduction and advances only on step()/resume(). Drives the
// scheduler directly with a capturing output channel and asserts the reduction
// sequence and the pause-per-step behavior.

/**
 * Renders a captured output value: a reduction's expression, else a raw value.
 * A traced run's opening line arrives as a trace-*start* (the program before it
 * reduces, rendered without the `-->` marker) and every later reduction as a
 * trace-output; both carry the same expression.
 */
function render(v: Value): string {
  if (isStructKind(v, 'trace-output') || isStructKind(v, 'trace-start')) {
    return expToString((v as { output: Value }).output as never)
  }
  return 'RAW:' + expToString(mkLit(v))
}

async function startStepping(src: string) {
  const { prog } = await Scheme.compile(src.trim())
  const fiber = new Fiber(required(prog, 'a compiled program'), Scheme.mkInitialEnv())
  const sched = new Scheduler()
  const steps: string[] = []
  const ch = {
    send: (v: Value) => steps.push(render(v)),
    report: (e: { message: string }) => steps.push('ERR:' + e.message),
    pushLevel: () => {
      /* nesting is not what these specs look at */
    },
    popLevel: () => {
      /* nesting is not what these specs look at */
    },
  }
  const id = 'test-run'
  let finished = false
  sched.schedule({
    id,
    fiber,
    out: ch,
    err: ch,
    isTracing: true,
    stepping: true,
    stepper: makeTraceStepper(),
    onComplete: () => {
      finished = true
    },
  })
  const resume = (mode: StepMode) => sched.resume(id, mode)
  const step = () => resume('step')
  return { sched, id, steps, resume, step, isFinished: () => finished }
}

// Recursive calls to a user-defined function are stepped *into*, so these
// traces unfold every nested call rather than skipping over it (#319).
const FACTORIAL =
  '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))\n(fact 3)'
const FACTORIAL_TRACE = [
  '(fact 3)',
  '(if (= 3 0) 1 (* 3 (fact (- 3 1))))',
  '(if #f 1 (* 3 (fact (- 3 1))))',
  '(* 3 (fact (- 3 1)))',
  '(* 3 (fact 2))',
  '(* 3 (if (= 2 0) 1 (* 2 (fact (- 2 1)))))',
  '(* 3 (if #f 1 (* 2 (fact (- 2 1)))))',
  '(* 3 (* 2 (fact (- 2 1))))',
  '(* 3 (* 2 (fact 1)))',
  '(* 3 (* 2 (if (= 1 0) 1 (* 1 (fact (- 1 1))))))',
  '(* 3 (* 2 (if #f 1 (* 1 (fact (- 1 1))))))',
  '(* 3 (* 2 (* 1 (fact (- 1 1)))))',
  '(* 3 (* 2 (* 1 (fact 0))))',
  '(* 3 (* 2 (* 1 (if (= 0 0) 1 (* 0 (fact (- 0 1)))))))',
  '(* 3 (* 2 (* 1 (if #t 1 (* 0 (fact (- 0 1)))))))',
  '(* 3 (* 2 (* 1 1)))',
  '(* 3 (* 2 1))',
  '(* 3 2)',
  '6',
]

const LIST_LENGTH =
  '(define len (lambda (l) (if (null? l) 0 (+ 1 (len (cdr l))))))\n(len (list 7 8))'
const LIST_LENGTH_TRACE = [
  '(len (list 7 8))',
  '(if (null? (list 7 8)) 0 (+ 1 (len (cdr (list 7 8)))))',
  '(if #f 0 (+ 1 (len (cdr (list 7 8)))))',
  '(+ 1 (len (cdr (list 7 8))))',
  '(+ 1 (len (list 8)))',
  '(+ 1 (if (null? (list 8)) 0 (+ 1 (len (cdr (list 8))))))',
  '(+ 1 (if #f 0 (+ 1 (len (cdr (list 8))))))',
  '(+ 1 (+ 1 (len (cdr (list 8)))))',
  '(+ 1 (+ 1 (len null)))',
  '(+ 1 (+ 1 (if (null? null) 0 (+ 1 (len (cdr null))))))',
  '(+ 1 (+ 1 (if #t 0 (+ 1 (len (cdr null))))))',
  '(+ 1 (+ 1 0))',
  '(+ 1 1)',
  '2',
]

describe('step once advances exactly one user-visible reduction', () => {
  test('factorial', async () => {
    const run = await startStepping(FACTORIAL)
    let guard = 0
    while (!run.isFinished() && guard++ < 50) {
      const before = run.steps.length
      await run.step()
      // each step emits at most one new reduction (one per click)
      expect(run.steps.length - before).toBeLessThanOrEqual(1)
    }
    expect(run.steps).toEqual(FACTORIAL_TRACE)
  })

  test('list length', async () => {
    const run = await startStepping(LIST_LENGTH)
    let guard = 0
    while (!run.isFinished() && guard++ < 50) {
      await run.step()
    }
    expect(run.steps).toEqual(LIST_LENGTH_TRACE)
  })
})

describe('resume modes', () => {
  test("'all' runs to completion, emitting the whole trace in one call", async () => {
    const run = await startStepping(FACTORIAL)
    await run.resume('all')
    expect(run.isFinished()).toBe(true)
    expect(run.steps).toEqual(FACTORIAL_TRACE)
  })

  test("'statement' stops at each statement boundary", async () => {
    const run = await startStepping('(+ 1 2)\n(* 3 4)')
    await run.resume('statement')
    expect(run.steps).toEqual(['(+ 1 2)', '3'])
    expect(run.isFinished()).toBe(false)
    await run.resume('statement')
    expect(run.steps).toEqual(['(+ 1 2)', '3', '(* 3 4)', '12'])
    expect(run.isFinished()).toBe(true)
  })
})

describe('abort and cancel', () => {
  test('pauseStepping downgrades an all-burst back to single-step (session stays alive)', async () => {
    const run = await startStepping(FACTORIAL)
    const p = run.resume('all')
    run.sched.pauseStepping(run.id) // abort the burst before it runs
    await p
    // the burst re-parked early rather than running to completion
    expect(run.isFinished()).toBe(false)
    expect(run.steps.length).toBeLessThan(FACTORIAL_TRACE.length)
    // the session is still alive: stepping continues to completion
    let guard = 0
    while (!run.isFinished() && guard++ < 50) {
      await run.step()
    }
    expect(run.steps).toEqual(FACTORIAL_TRACE)
  })

  test('cancelling a parked run reports cancellation and frees the gate', async () => {
    const run = await startStepping(FACTORIAL)
    await run.step() // park after the first reduction
    expect(run.steps).toEqual(['(fact 3)'])
    run.sched.cancelTask(run.id)
    expect(run.steps).toContain('ERR:Evaluation cancelled')
    // the gate is gone: a further resume is a no-op and emits nothing more
    await run.resume('step')
    expect(run.steps.filter((s) => !s.startsWith('ERR:'))).toEqual(['(fact 3)'])
  })
})

describe('robustness regressions', () => {
  test('overlapping resume() calls never leave a promise unresolved', async () => {
    // Two resumes in flight: the earlier one must be settled, not lost (which
    // would hang its promise forever). Promise.all would time out on a hang.
    const run = await startStepping(FACTORIAL)
    const p1 = run.resume('statement')
    const p2 = run.resume('all')
    await Promise.all([p1, p2])
    expect(run.isFinished()).toBe(true)
    expect(run.steps).toEqual(FACTORIAL_TRACE)
  })

  test('a stray step() while suspended in block-on does not double-run the fiber', async () => {
    // (+ 1 (block)) where (block) suspends via a controllable async action. The
    // step buttons stay enabled during the async wait, so a click can land while
    // the fiber is out of the run queue but not parked -- it must be a no-op,
    // not a re-schedule that double-runs the fiber.
    const sched = new Scheduler()
    const fiber = makeTestFiber([
      U.mkDisp([
        U.mkVar('+'),
        U.mkLit(1),
        U.mkVar('block'),
        U.mkAp(0),
        U.mkAp(2),
      ]),
    ])
    let resolveAction!: (v: unknown) => void
    const actionPromise = new Promise<unknown>((r) => {
      resolveAction = r
    })
    fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel([
      'block',
      (() => {
        throw new SuspendSignal(() => actionPromise)
      }) as unknown as Value,
    ])
    const steps: string[] = []
    const errs: string[] = []
    const ch = {
      send: (v: Value) => steps.push(render(v)),
      report: (e: { message: string }) => errs.push(e.message),
      pushLevel: () => {
        /* nesting is not what these specs look at */
      },
      popLevel: () => {
        /* nesting is not what these specs look at */
      },
    }
    let finished = false
    sched.schedule({
      id: 'r',
      fiber,
      out: ch,
      err: ch,
      isTracing: true,
      stepping: true,
      stepper: makeTraceStepper(),
      onComplete: () => {
        finished = true
      },
    })
    const done = sched.resume('r', 'all') // runs to the block-on and suspends there
    await new Promise((r) => setTimeout(r, 50))
    // suspended in block-on: these must be no-ops, not re-schedules
    sched.step('r')
    sched.step('r')
    resolveAction(5) // (+ 1 5)
    await done
    expect(errs).toEqual([])
    expect(finished).toBe(true)
    // exactly one correct result -- a double-run would corrupt the frame / value
    expect(steps.filter((s) => s === '6')).toHaveLength(1)
  })
})
