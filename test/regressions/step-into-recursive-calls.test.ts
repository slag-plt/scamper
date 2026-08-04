import { describe, expect, test } from 'vitest'
import * as Scheme from '../../src/scheme/index.js'
import { Fiber } from '../../src/lpm/fiber.js'
import { traceReductions } from '../../src/scheme/trace.js'

// Regression for #319: a reduction trace must step *into* calls to
// user-defined (module/local) functions -- including recursive calls -- while
// still treating imported-library/prelude calls atomically. Previously the
// stepper only rendered when back at the outermost frame, so any non-tail call
// (e.g. the recursive `(factorial n)` inside `(* n ...)`) was skipped: the
// trace jumped straight from `(* 5 (factorial 4))` to `(* 5 24)`.

/** The full reduction trace of `src` (drains the CLI/step-mode generator). */
async function reductionTrace(src: string): Promise<string[]> {
  const { prog, diagnostics } = await Scheme.compile(src.trim())
  expect(diagnostics).toEqual([])
  return [...traceReductions(new Fiber(prog!, Scheme.mkInitialEnv()))]
}

const FACT =
  '(define factorial\n' +
  '  (lambda (n)\n' +
  '    (if (zero? n) 1 (* n (factorial (- n 1))))))\n' +
  '(factorial 5)'

describe('recursive calls to user functions are stepped into (#319)', () => {
  test('the recursion unfolds instead of jumping over the recursive call', async () => {
    const trace = await reductionTrace(FACT)

    // The recursive call is formed as an argument to `*`...
    expect(trace).toContain('(* 5 (factorial 4))')
    // ...and then stepped *into*: nested unfoldings the old behavior could
    // never produce (it collapsed each call to its value in one jump).
    expect(trace).toContain('(* 5 (* 4 (* 3 (factorial 2))))')
    expect(trace).toContain('(* 5 (* 4 (* 3 (* 2 (* 1 (factorial 0))))))')

    // The specific bug: `(* 5 (factorial 4))` must NOT be followed directly by
    // `(* 5 24)` -- there are intervening steps that reduce `(factorial 4)`.
    const i = trace.indexOf('(* 5 (factorial 4))')
    expect(i).toBeGreaterThanOrEqual(0)
    expect(trace[i + 1]).not.toBe('(* 5 24)')
    expect(trace.at(-1)).toBe('120')
  })
})

describe('imported-library calls stay atomic in traces (#319)', () => {
  test('a library higher-order function reduces in a single step', async () => {
    // `map`/`fold` are defined in prelude.scm (Scheme closures), yet a trace
    // steps *over* them: no internal reduction (nor the user callback they
    // drive) is surfaced.
    expect(
      await reductionTrace(
        '(define double (lambda (x) (* x 2)))\n(map double (list 1 2 3))',
      ),
    ).toEqual(['(map double (list 1 2 3))', '(list 2 4 6)'])

    expect(await reductionTrace('(fold + 0 (list 1 2 3))')).toEqual([
      '(fold + 0 (list 1 2 3))',
      '6',
    ])
  })

  test('a user function is stepped into, but the library call inside it is not', async () => {
    const trace = await reductionTrace(
      '(define sum-doubled\n' +
        '  (lambda (xs) (fold + 0 (map (lambda (x) (* x 2)) xs))))\n' +
        '(sum-doubled (list 1 2 3))',
    )
    // We enter sum-doubled (its body shows), but map has already reduced
    // atomically to a concrete list -- its internals never appear.
    expect(trace).toContain('(fold + 0 (list 2 4 6))')
    expect(trace.some((s) => s.includes('cond'))).toBe(false)
    expect(trace.at(-1)).toBe('12')
  })
})
