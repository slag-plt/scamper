import { describe, expect, test } from 'vitest'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { expandExpr, expandProgram } from '../../src/scheme/expansion'
import { sugarExpr, sugarProgram } from '../../src/scheme/sugar'
import * as A from '../../src/scheme/ast'
import * as S from '../../src/scheme'
import { Fiber } from '../../src/lpm/fiber'
import { raiseFiber } from '../../src/scheme/raise'
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
  test('a single operand is recovered (provenance disambiguates it from or)', () => {
    expect(roundTrip('(and a)')).toBe('(and a)')
  })
  test('the empty and expands to a bare #t and stays #t', () => {
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
  test('a single operand is recovered (provenance disambiguates it from and)', () => {
    expect(roundTrip('(or a)')).toBe('(or a)')
  })
  test('the empty or expands to a bare #f and stays #f', () => {
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

describe('anonymous functions #(...)', () => {
  test('a single numbered parameter', () => {
    expect(roundTrip('#(+ %1 1)')).toBe('#(+ %1 1)')
  })
  test('% shorthand is recovered as its canonical %1', () => {
    // Expansion canonicalizes `%` to `%1`, so the recovered form uses `%1`.
    expect(roundTrip('#(+ % 1)')).toBe('#(+ %1 1)')
  })
  test('several parameters', () => {
    expect(roundTrip('#(list %1 %2 %3)')).toBe('#(list %1 %2 %3)')
  })
  test('a rest parameter', () => {
    expect(roundTrip('#(apply + %&)')).toBe('#(apply + %&)')
  })
  test('numbered plus rest parameters', () => {
    expect(roundTrip('#(cons %1 %&)')).toBe('#(cons %1 %&)')
  })
  test('a zero-operand thunk', () => {
    expect(roundTrip('#(g)')).toBe('#(g)')
  })
  test('the empty #()', () => {
    expect(roundTrip('#()')).toBe('#()')
  })
  test('an operand that is itself a derived form is also recovered', () => {
    expect(roundTrip('#(f (and %1 %2))')).toBe('#(f (and %1 %2))')
  })
  test('a #(...) nested in another derived form is recovered', () => {
    expect(roundTrip('(map #(* %1 %1) xs)')).toBe('(map #(* %1 %1) xs)')
  })

  // The body may be any parenthesized form, including the special forms.
  test('an if body', () => {
    expect(roundTrip('#(if %1 %2 %3)')).toBe('#(if %1 %2 %3)')
  })
  test('a let body', () => {
    expect(roundTrip('#(let ([x %1]) (+ x %2))')).toBe(
      '#(let ([x %1]) (+ x %2))',
    )
  })
  test('an and body (recovered through its expansion)', () => {
    expect(roundTrip('#(and %1 %2)')).toBe('#(and %1 %2)')
  })
  test('a cond body', () => {
    expect(roundTrip('#(cond [%1 1] [#t 2])')).toBe('#(cond [%1 1] [#t 2])')
  })
  test('a match body', () => {
    expect(roundTrip('#(match %1 [0 "z"] [_ "o"])')).toBe(
      '#(match %1 [0 "z"] [_ "o"])',
    )
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

describe('vector literal', () => {
  test('the (vector ...) it expands to is recovered as [...]', () => {
    expect(roundTrip('[1 2 3]')).toBe('[1 2 3]')
    expect(roundTrip('[]')).toBe('[]')
  })

  test('operands are sugared recursively', () => {
    expect(roundTrip('[(and a b) (or c d)]')).toBe('[(and a b) (or c d)]')
  })

  test('nests, and is recovered inside another derived form', () => {
    expect(roundTrip('[[1 2] [3 4]]')).toBe('[[1 2] [3 4]]')
    expect(roundTrip('(and [1] x)')).toBe('(and [1] x)')
  })

  test('a hand-written (vector ...) is left alone -- recovery is exact', () => {
    expect(roundTrip('(vector 1 2 3)')).toBe('(vector 1 2 3)')
  })
})

describe('map literal', () => {
  test('the (##mkObj## ...) it expands to is recovered as {...}', () => {
    expect(roundTrip('{"a" 1 "b" 2}')).toBe('{"a" 1 "b" 2}')
    expect(roundTrip('{}')).toBe('{}')
  })

  test('keys and values are sugared recursively', () => {
    expect(roundTrip('{(and a b) (or c d)}')).toBe('{(and a b) (or c d)}')
  })

  test('nests with vector literals', () => {
    expect(roundTrip('{"a" {"b" [1 2]}}')).toBe('{"a" {"b" [1 2]}}')
  })

  test('a hand-written (##mkObj## ...) is left alone -- recovery is exact', () => {
    expect(roundTrip('(##mkObj## "a" 1)')).toBe('(##mkObj## "a" 1)')
  })
})

// ---- Nothing to sugar (core forms pass through unchanged) ------------------

describe('nothing to sugar (core forms unchanged)', () => {
  test.for([
    ['a number', '42'],
    ['a string', '"hi"'],
    ['a boolean', '#t'],
    ['an identifier', 'x'],
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

// ---- Provenance makes recovery exact ---------------------------------------

describe('untagged look-alikes are never over-sugared', () => {
  // These core shapes are exactly what begin/cond expand to, but written by
  // hand they carry no provenance, so they must be left precisely as-is.
  test('a hand-written single wildcard-binding let stays a let', () => {
    expect(sugarStr('(let ([_ a]) b)')).toBe('(let ([_ a]) b)')
  })
  test('a hand-written if ending in the cond error call stays an if', () => {
    expect(sugarStr('(if a b (error "No matching clause in cond"))')).toBe(
      '(if a b (error "No matching clause in cond"))',
    )
  })
})

describe('provenance disambiguates otherwise-identical shapes', () => {
  test('(and e) and (or e) expand to the same core shape but recover distinctly', () => {
    // both expand to (if e #t #f); only the provenance tag tells them apart
    expect(roundTrip('(and a)')).toBe('(and a)')
    expect(roundTrip('(or a)')).toBe('(or a)')
  })
  test('the same (if e #t #f): tagged by expansion recovers, hand-written does not', () => {
    expect(roundTrip('(and a)')).toBe('(and a)')
    expect(sugarStr('(if a #t #f)')).toBe('(if a #t #f)')
  })
  test('a tagged wildcard-let recovers, an identical hand-written one does not', () => {
    expect(roundTrip('(begin a b)')).toBe('(begin a b)')
    expect(sugarStr('(let ([_ a]) b)')).toBe('(let ([_ a]) b)')
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

// ---- Recovery through the LPM (provenance survives codegen -> ops -> raise) --

describe('provenance survives the whole AST -> op -> AST round trip', () => {
  // Collect the sugared reconstruction at every VM step of `src`.
  async function trace(src: string): Promise<string[]> {
    const { prog, diagnostics } = await S.compile(src)
    expect(diagnostics).toEqual([])
    const fiber = new Fiber(prog!, S.mkInitialEnv())
    const out: string[] = []
    while (!fiber.isDone()) {
      fiber.step()
      if (fiber.frames.length > 0) {
        out.push(A.expToString(sugarExpr(raiseFiber(fiber))))
      }
    }
    return out
  }

  test('an `and` is recovered mid-run (codegen tagged the if/lit ops)', async () => {
    // once the first operand reduces to #t, raising + sugaring shows the `and`
    expect(await trace('(and (< 1 2) (< 3 4))')).toContain('(and #t (< 3 4))')
  })

  test('an `or` is recovered mid-run', async () => {
    expect(await trace('(or (< 5 1) (< 3 4))')).toContain('(or #f (< 3 4))')
  })

  test('a `cond` is recovered mid-run', async () => {
    expect(await trace('(cond [(< 5 1) 1] [(< 3 4) 2])')).toContain(
      '(cond [#f 1] [(< 3 4) 2])',
    )
  })

  test('a `begin` is recovered mid-run (the let op kept its begin tag)', async () => {
    // the wildcard-binding let carries provenance through codegen and the
    // idx-threaded continuation ops, so a trace shows `begin`, not `let`
    const forms = await trace('(begin (+ 1 1) (+ 2 2))')
    expect(forms.some((f) => f.startsWith('(begin '))).toBe(true)
  })

  test('a `#(...)` is recovered before its closure op executes (cls kept its anon-fn tag)', async () => {
    // Before the `cls` op runs, raising + sugaring shows the anonymous function
    // rather than a bare `(lambda ...)`; codegen tagged the cls op `anon-fn`.
    const forms = await trace('(map #(* %1 %1) (list 1 2 3))')
    expect(forms.some((f) => f.includes('#(* %1 %1)'))).toBe(true)
  })

  test('an ordinary lambda never renders as #(...) (the anon-fn tag does not leak)', async () => {
    const forms = await trace('(map (lambda (x) (* x x)) (list 1 2 3))')
    expect(forms.every((f) => !f.includes('#('))).toBe(true)
  })
})
