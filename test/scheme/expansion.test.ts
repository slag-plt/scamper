import { expect, test, describe } from 'vitest'
import { expandExpr } from '../../src/scheme/expansion.js'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge.js'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic.js'
import * as A from '../../src/scheme/ast.js'

/** Parse a single bare-expression statement, expand it, and return the result. */
function expand(src: string): A.Exp {
  const errors: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(errors, src)
  expect(errors, `parse errors for ${JSON.stringify(src)}`).toEqual([])
  const stmt = prog[0]
  if (!A.isStmtExp(stmt)) throw new Error('expected a bare expression')
  return expandExpr(stmt.expr)
}

/** The expanded form of `src`, rendered as text (ranges/provenance ignored). */
function expandStr(src: string): string {
  return A.expToString(expand(src))
}

// Expansion tags every node it inserts with the derived form's name (its
// provenance) so sugaring can recover the form exactly. These helpers build the
// expected tagged nodes for the structural comparisons below.
const andIf = (g: A.Exp, t: A.Exp, e: A.Exp) => A.mkIf(g, t, e, undefined, 'and')
const andBool = (v: boolean) => A.mkLit(v, undefined, 'and')
const orIf = (g: A.Exp, t: A.Exp, e: A.Exp) => A.mkIf(g, t, e, undefined, 'or')
const orBool = (v: boolean) => A.mkLit(v, undefined, 'or')
const condIf = (g: A.Exp, t: A.Exp, e: A.Exp) => A.mkIf(g, t, e, undefined, 'cond')
const condSentinel = () =>
  A.mkApp(A.mkId('error'), [A.mkLit('No matching clause in cond')], undefined, 'cond')

describe('Expanded expressions', () => {
  test('and', () => {
    const actual = expandExpr(A.mkAnd([A.mkId('X'), A.mkId('Y'), A.mkId('Z')]))
    const expected =
      andIf(A.mkId('X'),
        andIf(A.mkId('Y'),
          andIf(A.mkId('Z'), andBool(true), andBool(false)),
          andBool(false)),
        andBool(false))
    expect(actual).toEqual(expected)
  })

  test('or', () => {
    const actual = expandExpr(A.mkOr([A.mkId('X'), A.mkId('Y'), A.mkId('Z')]))
    const expected =
      orIf(A.mkId('X'), orBool(true),
        orIf(A.mkId('Y'), orBool(true),
          orIf(A.mkId('Z'), orBool(true), orBool(false))))
    expect(actual).toEqual(expected)
  })

  test('cond', () => {
    const actual = expandExpr(A.mkCond([
      { test: A.mkId('X'), body: A.mkId('A') },
      { test: A.mkId('Y'), body: A.mkId('B') },
      { test: A.mkId('Z'), body: A.mkId('C') }
    ]))
    const expected =
      condIf(A.mkId('X'), A.mkId('A'),
        condIf(A.mkId('Y'), A.mkId('B'),
          condIf(A.mkId('Z'), A.mkId('C'), condSentinel())))
    expect(actual).toEqual(expected)
  })
})

describe('Anonymous functions #(...)', () => {
  test('a single numbered parameter', () => {
    expect(expandStr('#(+ %1 1)')).toBe('(lambda (%1) (+ %1 1))')
  })

  test('% is shorthand for %1', () => {
    expect(expandStr('#(+ % 1)')).toBe('(lambda (%1) (+ %1 1))')
  })

  test('% and %1 name the same parameter (arity stays 1)', () => {
    expect(expandStr('#(* % %1)')).toBe('(lambda (%1) (* %1 %1))')
  })

  test('several distinct parameters', () => {
    expect(expandStr('#(list %1 %2 %3)')).toBe(
      '(lambda (%1 %2 %3) (list %1 %2 %3))',
    )
  })

  test('the arity is the largest index used; skipped indices become unused params', () => {
    expect(expandStr('#(f %3)')).toBe('(lambda (%1 %2 %3) (f %3))')
  })

  test('%& alone is a rest parameter (arity 0)', () => {
    expect(expandStr('#(apply + %&)')).toBe('(lambda (& %&) (apply + %&))')
  })

  test('numbered parameters plus a rest parameter', () => {
    expect(expandStr('#(cons %1 %&)')).toBe('(lambda (%1 & %&) (cons %1 %&))')
  })

  test('no parameters yields a thunk that applies its single operand', () => {
    expect(expandStr('#(g)')).toBe('(lambda () (g))')
  })

  test('an empty #() expands to a lambda returning null', () => {
    expect(expandStr('#()')).toBe('(lambda () null)')
  })

  test('operands may themselves be compound (derived) forms', () => {
    expect(expandStr('#(f (if %1 %2 %3))')).toBe(
      '(lambda (%1 %2 %3) (f (if %1 %2 %3)))',
    )
    // `and` is a derived form; the % refs inside it still count toward arity.
    expect(expandStr('#(f (and %1 %2))')).toBe(
      '(lambda (%1 %2) (f (if %1 (if %2 #t #f) #f)))',
    )
  })

  test('parameters referenced inside a nested lambda still count toward arity', () => {
    expect(expandStr('#(map (lambda (x) (+ x %2)) %1)')).toBe(
      '(lambda (%1 %2) (map (lambda (x) (+ x %2)) %1))',
    )
  })

  test('the body may be a special form, wrapped verbatim in the lambda', () => {
    // `if`/`let` are core forms, so they survive expansion unchanged.
    expect(expandStr('#(if % 1 2)')).toBe('(lambda (%1) (if %1 1 2))')
    expect(expandStr('#(let ([x %1]) (+ x %2))')).toBe(
      '(lambda (%1 %2) (let ([x %1]) (+ x %2)))',
    )
  })

  test('a derived form as the body expands through, still collecting its % refs', () => {
    // `and` is itself expanded, and the % refs inside it drive the arity.
    expect(expandStr('#(and % %2)')).toBe(
      '(lambda (%1 %2) (if %1 (if %2 #t #f) #f))',
    )
  })

  test('the expanded lambda is tagged with anon-fn provenance', () => {
    const lam = expand('#(+ %1 1)')
    expect(lam.tag).toBe('lam')
    expect(lam.provenance).toBe('anon-fn')
  })
})
