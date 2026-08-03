import { describe, expect, test } from 'vitest'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { expandExpr, expandProgram } from '../../src/scheme/expansion'
import { sugarExpr, sugarProgram } from '../../src/scheme/sugar'
import * as A from '../../src/scheme/ast'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// ---- Helpers ---------------------------------------------------------------

/** Parse a single bare-expression statement and return its expression. */
function parseExp(src: string): A.Exp {
  const errors: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(errors, src)
  expect(errors).toEqual([])
  const stmt = prog[0]
  if (!A.isStmtExp(stmt)) {
    throw new Error(`expected a bare expression, got tag "${stmt.tag}"`)
  }
  return stmt.expr
}

/** Sugar a source expression as-is (no expansion): tests that core forms with
 * no derived-form shape are left untouched. */
function sugarStr(src: string): string {
  return A.expToString(sugarExpr(parseExp(src)))
}

/** The full round trip: parse -> expand (desugar) -> sugar -> print. For a
 * well-formed derived form this recovers it. */
function roundTrip(src: string): string {
  return A.expToString(sugarExpr(expandExpr(parseExp(src))))
}

// ---- Recovering each derived form ------------------------------------------

describe('and', () => {
  test('two operands', () => {
    expect(roundTrip('(and a b)')).toBe('(and a b)')
  })
  test('several operands', () => {
    expect(roundTrip('(and a b c d)')).toBe('(and a b c d)')
  })
  test('a single operand is left as its (ambiguous) if-shape', () => {
    // (and e) and (or e) both expand to (if e #t #f), so neither is recovered
    expect(roundTrip('(and a)')).toBe('(if a #t #f)')
  })
  test('the empty and expands to #t and stays #t', () => {
    expect(roundTrip('(and)')).toBe('#t')
  })
  test('operands may be literals', () => {
    expect(roundTrip('(and #t x)')).toBe('(and #t x)')
    expect(roundTrip('(and x #t)')).toBe('(and x #t)')
  })
})

describe('or', () => {
  test('two operands', () => {
    expect(roundTrip('(or a b)')).toBe('(or a b)')
  })
  test('several operands', () => {
    expect(roundTrip('(or a b c d)')).toBe('(or a b c d)')
  })
  test('a single operand is left as its (ambiguous) if-shape', () => {
    expect(roundTrip('(or a)')).toBe('(if a #t #f)')
  })
  test('the empty or expands to #f and stays #f', () => {
    expect(roundTrip('(or)')).toBe('#f')
  })
})

describe('begin', () => {
  test('two expressions', () => {
    expect(roundTrip('(begin a b)')).toBe('(begin a b)')
  })
  test('several expressions', () => {
    expect(roundTrip('(begin a b c d)')).toBe('(begin a b c d)')
  })
  test('a single expression expands away and stays a bare expression', () => {
    expect(roundTrip('(begin a)')).toBe('a')
  })
})

describe('cond', () => {
  test('a single clause', () => {
    expect(roundTrip('(cond [a b])')).toBe('(cond [a b])')
  })
  test('several clauses', () => {
    expect(roundTrip('(cond [a b] [c d] [e f])')).toBe('(cond [a b] [c d] [e f])')
  })
  test('the empty cond expands to the fall-through error and stays that', () => {
    expect(roundTrip('(cond)')).toBe('(error "No matching clause in cond")')
  })
})

// ---- Nested derived forms (careful recursion) ------------------------------

describe('nested derived forms are recovered, not lost', () => {
  test('an or inside an and operand', () => {
    expect(roundTrip('(and (or a b) c)')).toBe('(and (or a b) c)')
  })
  test('an and inside an or operand', () => {
    expect(roundTrip('(or (and a b) c)')).toBe('(or (and a b) c)')
  })
  test('a derived form as a *later* operand (not just the first)', () => {
    expect(roundTrip('(and a (or b c))')).toBe('(and a (or b c))')
    expect(roundTrip('(or a (and b c))')).toBe('(or a (and b c))')
  })
  test('a nested same-form operand is not flattened away', () => {
    // (and (and a b) c) and (and a b c) have different expansions
    expect(roundTrip('(and (and a b) c)')).toBe('(and (and a b) c)')
    expect(roundTrip('(and a b c)')).toBe('(and a b c)')
    expect(roundTrip('(or (or a b) c)')).toBe('(or (or a b) c)')
  })
  test('a begin inside a cond branch body', () => {
    expect(roundTrip('(cond [a (begin b c)])')).toBe('(cond [a (begin b c)])')
  })
  test('derived forms inside a cond test and body', () => {
    expect(roundTrip('(cond [(and a b) (or c d)] [e f])')).toBe(
      '(cond [(and a b) (or c d)] [e f])',
    )
  })
  test('derived forms sequenced inside a begin', () => {
    expect(roundTrip('(begin (and a b) (or c d))')).toBe(
      '(begin (and a b) (or c d))',
    )
  })
  test('a cond inside an and, and an and inside a cond', () => {
    expect(roundTrip('(and (cond [x y]) z)')).toBe('(and (cond [x y]) z)')
    expect(roundTrip('(cond [x (and y z)])')).toBe('(cond [x (and y z)])')
  })
  test('deep nesting across several forms', () => {
    const src = '(and (or a (and b c)) (cond [d (begin e f)]))'
    expect(roundTrip(src)).toBe(src)
  })
  test('derived forms in the operator and arguments of an application', () => {
    expect(roundTrip('((and f g) (or a b) c)')).toBe('((and f g) (or a b) c)')
  })
  test('a derived form in a lambda body', () => {
    expect(roundTrip('(lambda (x) (and x y))')).toBe('(lambda (x) (and x y))')
  })
  test('a derived form nested inside a plain if that is not itself a derived form', () => {
    // the guard is an `and` but the whole if is neither and/or/cond
    expect(roundTrip('(if (and a b) c d)')).toBe('(if (and a b) c d)')
  })
  test('a derived form in a let binding value and body', () => {
    expect(roundTrip('(let ([x (and a b)]) (or x c))')).toBe(
      '(let ([x (and a b)]) (or x c))',
    )
  })
  test('a derived form in a match scrutinee and branch body', () => {
    expect(roundTrip('(match (or a b) [0 (and c d)] [_ e])')).toBe(
      '(match (or a b) [0 (and c d)] [_ e])',
    )
  })
})

// ---- Nothing to sugar (core forms pass through unchanged) ------------------

describe('nothing to sugar (core forms unchanged)', () => {
  test.for([
    ['a number', '42'],
    ['a string', '"hi"'],
    ['a boolean', '#t'],
    ['an identifier', 'x'],
    ['a quote', "'(1 2 3)"],
    ['an application', '(f a b)'],
    ['a lambda', '(lambda (x y) (+ x y))'],
    ['a plain if', '(if a b c)'],
    ['a let', '(let ([x 1] [y 2]) (+ x y))'],
    ['a match', '(match x [1 a] [(cons h t) h] [_ b])'],
    ['nested applications', '(+ (* 2 3) (- 5 1))'],
  ])('%s', ([, src]) => {
    expect(sugarStr(src)).toBe(sugarStr(src)) // parse is stable
    expect(A.expToString(sugarExpr(parseExp(src)))).toBe(
      A.expToString(parseExp(src)),
    )
  })
})

// ---- Not over-sugaring ambiguous / look-alike core shapes ------------------

describe('does not over-sugar look-alike core shapes', () => {
  test('(if e #t #f) is left alone (ambiguous single and/or)', () => {
    expect(sugarStr('(if a #t #f)')).toBe('(if a #t #f)')
  })
  test('(if e X #f) with a non-and-tail then is a plain if', () => {
    expect(sugarStr('(if a b #f)')).toBe('(if a b #f)')
  })
  test('(if e #t X) with a non-or-tail else is a plain if', () => {
    expect(sugarStr('(if a #t b)')).toBe('(if a #t b)')
  })
  test('a nested if that does not end in the cond sentinel stays nested ifs', () => {
    expect(sugarStr('(if a b (if c d e))')).toBe('(if a b (if c d e))')
  })
  test('a multi-binding let (even with a wildcard first) is not a begin', () => {
    expect(sugarStr('(let ([_ a] [x b]) x)')).toBe('(let ([_ a] [x b]) x)')
  })
  test('a non-wildcard single-binding let is not a begin', () => {
    expect(sugarStr('(let ([x a]) x)')).toBe('(let ([x a]) x)')
  })
})

// ---- Look-alikes that are recovered (documented equivalences) --------------

describe('semantically-equivalent core shapes are recovered', () => {
  test('a single wildcard-binding let is recovered as a begin', () => {
    expect(sugarStr('(let ([_ a]) b)')).toBe('(begin a b)')
  })
  test('an if ending in the cond sentinel is recovered as a cond', () => {
    expect(sugarStr('(if a b (error "No matching clause in cond"))')).toBe(
      '(cond [a b])',
    )
  })
})

// ---- Idempotence -----------------------------------------------------------

describe('sugaring is idempotent', () => {
  test.for([
    '(and a b)',
    '(or (and a b) c)',
    '(cond [a (begin b c)] [d e])',
    '(begin (and a b) (or c d))',
    '(if a b c)',
    '(let ([x 1]) x)',
  ])('%s', (src) => {
    const once = sugarExpr(expandExpr(parseExp(src)))
    const twice = sugarExpr(once)
    expect(A.expToString(twice)).toBe(A.expToString(once))
  })
})

// ---- Statements and programs -----------------------------------------------

describe('statements and programs', () => {
  function roundTripProg(src: string): string {
    return sugarProgram(expandProgram(parseProgramFromSource([], src)))
      .map(A.stmtToString)
      .join('\n')
  }

  test('a derived form inside a define value', () => {
    expect(roundTripProg('(define f (and a b))')).toBe('(define f (and a b))')
  })

  test('a derived form inside a display', () => {
    expect(roundTripProg('(display (or a b))')).toBe('(display (or a b))')
  })

  test('a derived form as a bare top-level expression', () => {
    expect(roundTripProg('(cond [a b] [c d])')).toBe('(cond [a b] [c d])')
  })

  test('imports pass through unchanged', () => {
    expect(roundTripProg('(import image)')).toBe('(import image)')
  })

  test('a multi-statement program sugars each statement', () => {
    const src = '(define f (and a b))\n(display (or c d))\n(begin e g)'
    expect(roundTripProg(src)).toBe(
      '(define f (and a b))\n(display (or c d))\n(begin e g)',
    )
  })
})
