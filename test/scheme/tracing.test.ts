import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { runProgram } from '../harness.js'
import * as Scheme from '../../src/scheme/index.js'
import { Fiber } from '../../src/lpm/fiber.js'
import { diagnosticToError } from '../../src/scheme/diagnostic'
import { LoggingChannel } from '../../src/lpm/output/index.js'
import { Scheduler } from '../../src/lpm/scheduler.js'
import { makeTraceStepper, traceReductions } from '../../src/scheme/trace.js'

// runProgram (test/harness.ts) steps a fiber directly and has no tracing
// toggle, so tracing needs the real Scheduler wired up with isTracing and a
// stepper (as Scamper.execute does) -- that's what emits the reduction trace
// (see the isTracing branch in src/lpm/scheduler.ts).
async function runProgramTraced(src: string, isTracing = true): Promise<string[]> {
  src = src.trim()
  const out = new LoggingChannel()
  const env = Scheme.mkInitialEnv()
  const { prog, diagnostics } = await Scheme.compile(src)
  diagnostics.forEach((d) => { out.report(diagnosticToError(d)) })
  if (out.log.length !== 0) {
    return out.log as string[]
  }
  if (prog === undefined) {
    throw new Error('compile produced no program and no logged errors')
  }
  const fiber = new Fiber(prog, env)
  const sched = new Scheduler()
  await new Promise<void>((resolve) => {
    sched.schedule({
      id: crypto.randomUUID(),
      fiber,
      out,
      err: out,
      isTracing,
      stepper: isTracing ? makeTraceStepper() : undefined,
      onComplete: resolve,
    })
  })
  return out.log as string[]
}

beforeEach(() => {
  vi.stubGlobal('window', {
    AudioContext: vi.fn(),
  })
})
afterEach(() => {
  vi.unstubAllGlobals()
})

test('traces each define with its defined value', async () => {
  expect(
    await runProgramTraced(`
      (define x 5)
      (define y 10)
    `),
  ).toEqual(['--> 5', '--> 10'])
})

test('a builtin import produces no reduction steps', async () => {
  expect(await runProgramTraced('(import music)')).toEqual([])
})

test('a traced expression shows its reduction, ending at the value', async () => {
  // The scheduler now emits the reduction trace (raise + sugar per step), not
  // the old coarse `--> lastResult`.
  expect(await runProgramTraced('(+ 1 2)')).toEqual(['--> (+ 1 2)', '--> 3'])
})

test('a traced program renders every statement as arrow-prefixed reductions', async () => {
  // In trace mode the output *is* the reduction trace: each statement's steps
  // (and its value) are arrow-prefixed, rather than raw output plus annotations.
  const traced = await runProgramTraced('(define x 5)\n(display "hi")\n(+ x 1)')
  expect(traced.every((line) => line.startsWith('--> '))).toBe(true)
  expect(traced).toEqual(['--> 5', '--> "hi"', '--> (+ 5 1)', '--> 6'])
})

test('tracing disabled never emits arrow-prefixed lines', async () => {
  const result = await runProgramTraced('(define x 5)\n(+ x 1)', false)
  expect(result.some((line) => line.startsWith('--> '))).toBe(false)
  expect(result).toEqual(['6'])
})

// ---- Stepwise reduction traces (raise + sugar) -----------------------------
//
// The above scheduler traces are coarse (they emit `fiber.lastResult`, which
// only advances per statement). The *reduction* trace -- the readable
// program-state-per-step sequence the raise/sugar/provenance machinery was
// built for -- is produced by traceReductions (src/scheme/trace.ts), a
// generator that raises + sugars the fiber at each top-level step. Draining it
// here gives the full sequence; the CLI's --trace drives the same generator.
async function reductionTrace(src: string): Promise<string[]> {
  const { prog, diagnostics } = await Scheme.compile(src.trim())
  expect(diagnostics).toEqual([])
  return [...traceReductions(new Fiber(prog!, Scheme.mkInitialEnv()))]
}

describe('stepwise reduction traces, by construct', () => {
  test('a literal does not reduce', async () => {
    expect(await reductionTrace('42')).toEqual(['42'])
  })

  test('application reduces innermost operands first, then the call', async () => {
    expect(await reductionTrace('(+ 1 (* 2 3))')).toEqual([
      '(+ 1 (* 2 3))',
      '(+ 1 6)',
      '7',
    ])
  })

  test('if reduces its guard, then selects the matching branch', async () => {
    expect(await reductionTrace('(if (< 1 2) 10 20)')).toEqual([
      '(if (< 1 2) 10 20)',
      '(if #t 10 20)',
      '10',
    ])
  })

  test('cond peels clauses until a test holds', async () => {
    expect(await reductionTrace('(cond [(< 5 1) 1] [(< 1 5) 2])')).toEqual([
      '(cond [(< 5 1) 1] [(< 1 5) 2])',
      '(cond [#f 1] [(< 1 5) 2])',
      '(cond [(< 1 5) 2])',
      '(cond [#t 2])',
      '2',
    ])
  })

  test('and short-circuits through its operands', async () => {
    expect(await reductionTrace('(and (< 1 2) (< 3 4))')).toEqual([
      '(and (< 1 2) (< 3 4))',
      '(and #t (< 3 4))',
      '(and (< 3 4))',
      '(and #t)',
      '#t',
    ])
  })

  test('or short-circuits through its operands', async () => {
    expect(await reductionTrace('(or (< 5 1) (< 3 4))')).toEqual([
      '(or (< 5 1) (< 3 4))',
      '(or #f (< 3 4))',
      '(or (< 3 4))',
      '(or #t)',
      '#t',
    ])
  })

  test('let fills its binders left-to-right (letrec progress)', async () => {
    expect(await reductionTrace('(let ([x 2] [y 3]) (+ x y))')).toEqual([
      '(let ([x 2] [y 3]) (+ x y))',
      '(let ([y 3]) (+ 2 y))',
      '(+ 2 3)',
      '5',
    ])
  })

  test('begin sequences its expressions, discarding all but the last', async () => {
    expect(await reductionTrace('(begin (+ 1 1) (+ 2 2))')).toEqual([
      '(begin (+ 1 1) (+ 2 2))',
      '(begin 2 (+ 2 2))',
      '(+ 2 2)',
      '4',
    ])
  })

  test('a function application substitutes its argument into the body', async () => {
    expect(
      await reductionTrace('(define sq (lambda (x) (* x x)))\n(sq 4)'),
    ).toEqual(['(sq 4)', '(* 4 4)', '16'])
  })
})

describe('reduction traces show derived forms sugared, not desugared', () => {
  // The whole point of sugar + provenance in tracing: a step shows `(and ...)`,
  // not the `(if ... #t #f)` chain it desugars to.
  test('and never surfaces its if-expansion', async () => {
    const trace = await reductionTrace('(and (< 1 2) (< 3 4))')
    expect(trace.every((s) => !s.includes('(if '))).toBe(true)
    expect(trace.some((s) => s.startsWith('(and '))).toBe(true)
  })

  test('cond never surfaces its if-expansion', async () => {
    const trace = await reductionTrace('(cond [(< 5 1) 1] [(< 1 5) 2])')
    expect(trace.every((s) => !s.includes('(if '))).toBe(true)
    expect(trace.some((s) => s.startsWith('(cond '))).toBe(true)
  })

  test('begin never surfaces its let-expansion', async () => {
    const trace = await reductionTrace('(begin (+ 1 1) (+ 2 2))')
    expect(trace.every((s) => !s.includes('(let '))).toBe(true)
    expect(trace.some((s) => s.startsWith('(begin '))).toBe(true)
  })
})

describe('stepwise reduction traces of substantial programs', () => {
  // A recursive call to a user-defined function is stepped *into*, so the
  // trace unfolds every nested call rather than skipping over it (#319).
  test('factorial unfolds the recursion and multiplies back up', async () => {
    const src =
      '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))\n(fact 3)'
    expect(await reductionTrace(src)).toEqual([
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
    ])
  })

  test('list length walks the list, adding 1 per element', async () => {
    const src =
      '(define len (lambda (l) (if (null? l) 0 (+ 1 (len (cdr l))))))\n(len (list 7 8))'
    expect(await reductionTrace(src)).toEqual([
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
    ])
  })

  test('a reduction trace starts at the source and ends at the untraced value', async () => {
    const src = '(+ (* 2 3) (- 10 6))'
    const trace = await reductionTrace(src)
    expect(trace[0]).toBe(src)
    expect(trace[trace.length - 1]).toBe((await runProgram(src))[0])
  })

  test('every statement value is shown, not just the last (regression)', async () => {
    // Each statement's value is emitted at its own boundary; earlier a single
    // post-loop emit dropped all but the final program value.
    expect(await reductionTrace('(+ 1 2)\n(* 3 4)')).toEqual([
      '(+ 1 2)',
      '3',
      '(* 3 4)',
      '12',
    ])
  })
})

// Vector and map literals expand into applications, so a reduction trace would
// show `(vector ...)` / `(##mkObj## ...)` unless sugaring recovers the surface
// form from the provenance tag it carries (see sugar.ts).
describe('vector and map literals in a reduction trace (#334)', () => {
  // N.B., the final line of each trace is the *value*, which prints in its own
  // form ((vector ...) / { "k" : v }) rather than the source form -- only the
  // reduction steps in between are surface syntax.
  test('a vector literal reduces as [...], never as (vector ...)', async () => {
    expect(await runProgramTraced('[1 (+ 1 1)]')).toEqual([
      '--> [1 (+ 1 1)]',
      '--> [1 2]',
      '--> (vector 1 2)',
    ])
  })

  test('a map literal reduces as {...}, never as (##mkObj## ...)', async () => {
    expect(await runProgramTraced('{"a" (+ 1 1)}')).toEqual([
      '--> {"a" (+ 1 1)}',
      '--> {"a" 2}',
      '--> { "a" : 2 }',
    ])
  })
})
