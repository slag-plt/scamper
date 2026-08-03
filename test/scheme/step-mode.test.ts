import { describe, expect, test } from 'vitest'
import * as Scheme from '../../src/scheme/index.js'
import { Fiber } from '../../src/lpm/fiber.js'
import { Scheduler } from '../../src/lpm/scheduler.js'
import type { StepMode } from '../../src/lpm/scheduler.js'
import { makeTraceStepper } from '../../src/scheme/trace.js'
import { expToString, mkLit } from '../../src/scheme/ast.js'
import { isStructKind } from '../../src/lpm/util.js'
import type { Value } from '../../src/lpm/lang.js'

// End-to-end tests for the scheduler's step mode: a run pauses ("parks") after
// each user-visible reduction and advances only on step()/resume(). Drives the
// scheduler directly with a capturing output channel and asserts the reduction
// sequence and the pause-per-step behavior.

/** Renders a captured output value: a trace-output's expression, else a raw value. */
function render(v: Value): string {
  if (isStructKind(v, 'trace-output')) {
    return expToString((v as { output: Value }).output as never)
  }
  return 'RAW:' + expToString(mkLit(v))
}

async function startStepping(src: string) {
  const { prog } = await Scheme.compile(src.trim())
  const fiber = new Fiber(prog!, Scheme.mkInitialEnv())
  const sched = new Scheduler()
  const steps: string[] = []
  const ch = {
    send: (v: Value) => steps.push(render(v)),
    report: (e: { message: string }) => steps.push('ERR:' + e.message),
    pushLevel: () => {},
    popLevel: () => {},
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

const FACTORIAL =
  '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))\n(fact 3)'
const FACTORIAL_TRACE = [
  '(fact 3)',
  '(if (= 3 0) 1 (* 3 (fact (- 3 1))))',
  '(if #f 1 (* 3 (fact (- 3 1))))',
  '(* 3 (fact (- 3 1)))',
  '(* 3 (fact 2))',
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
