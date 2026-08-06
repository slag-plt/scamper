import { describe, expect, test } from 'vitest'
import * as A from '../../../src/scheme/ast'
import { reservedWords } from '../../../src/scheme/reserved-words'
import { expectParses, parse } from './test-utils'

describe('lezer-bridge parsing', () => {
  test('core forms', () => {
    expectParses('(lambda (x y) (+ x y))')
    expectParses('(let ([x 1] [y 2]) (+ x y))')
    expectParses('(let* ([x 1] [y (+ x 1)]) y)')
    expectParses('(begin 1 2 3)')
    expectParses('(if #t 1 2)')
    expectParses('(and)')
    expectParses('(and 1 2 3)')
    expectParses('(or)')
    expectParses('(or 1 2 3)')
    expectParses('(export a b c)')
    expectParses('(define-export x 1)')
  })

  test('rest parameters use Clojure-style "&", including zero fixed params (#272)', () => {
    // A rest parameter after one or more fixed parameters.
    const one = parse('(lambda (x & rest) rest)')
    expect(one.errors).toEqual([])
    const s1 = one.prog[0]
    expect(s1.tag).toBe('stmtexp')
    if (s1.tag !== 'stmtexp' || s1.expr.tag !== 'lam') return
    expect(s1.expr.params.map((p) => p.name)).toEqual(['x'])
    expect(s1.expr.restParam?.name).toBe('rest')

    // A rest-only parameter list -- the case Scheme's dotted syntax couldn't
    // express unambiguously.
    const zero = parse('(lambda (& rest) rest)')
    expect(zero.errors).toEqual([])
    const s2 = zero.prog[0]
    expect(s2.tag).toBe('stmtexp')
    if (s2.tag !== 'stmtexp' || s2.expr.tag !== 'lam') return
    expect(s2.expr.params).toEqual([])
    expect(s2.expr.restParam?.name).toBe('rest')

    expectParses('(lambda (a b c & rest) rest)')
  })

  test('the old Scheme dotted rest syntax is no longer accepted (#272)', () => {
    // "." is not a valid identifier or rest marker anymore.
    expect(parse('(lambda (x . y) y)').errors.length).toBeGreaterThan(0)
    expect(parse('(lambda (. y) y)').errors.length).toBeGreaterThan(0)
  })

  test('a rest marker requires exactly one trailing identifier (#272)', () => {
    // "&" with no rest name, or more than one name after it, is malformed.
    expect(parse('(lambda (&) x)').errors.length).toBeGreaterThan(0)
    expect(parse('(lambda (x &) x)').errors.length).toBeGreaterThan(0)
    expect(parse('(lambda (x & y z) x)').errors.length).toBeGreaterThan(0)
  })

  test('curly braces are no longer an alternate spelling of parens (#334)', () => {
    // "{" now opens a map literal, so these are either a map (when they happen
    // to hold an even number of elements) or a malformed one -- never a call.
    expect(parse('{+ 1 2}').errors.length).toBeGreaterThan(0)
    expect(parse('{define f (lambda (x) x)}').errors.length).toBeGreaterThan(0)
    const { prog, errors } = parse('{* 3}')
    expect(errors).toEqual([])
    expect(prog[0].tag).toBe('stmtexp')
    if (prog[0].tag !== 'stmtexp') return
    expect(prog[0].expr.tag).toBe('obj')
  })

  test('quotation has been removed from the language (#334)', () => {
    // "'" is not part of any token, so the shorthand is a syntax error...
    expect(parse("'(1 2 3)").errors.length).toBeGreaterThan(0)
    expect(parse("'a").errors.length).toBeGreaterThan(0)
    // ...and "quote" is now just an ordinary identifier, so (quote x) parses as
    // a plain application (which fails later, at scope-check/run time).
    const { prog, errors } = parse('(quote x)')
    expect(errors).toEqual([])
    expect(prog[0].tag).toBe('stmtexp')
    if (prog[0].tag !== 'stmtexp') return
    expect(prog[0].expr.tag).toBe('app')
  })

  test('vector literals are sub-expressions to evaluate, not literal data (#325)', () => {
    const { prog, errors } = parse('(display [1 2 3])')
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe('display')
    if (stmt.tag !== 'display') return
    expect(stmt.value.tag).toBe('vec')
    if (stmt.value.tag !== 'vec') return
    expect(stmt.value.exps.map((e) => e.tag)).toEqual(['lit', 'lit', 'lit'])

    expectParses('[]')
    expectParses('(display [(+ 1 2) "x" #t])')
    // An identifier inside a vector literal is a real variable reference.
    const v = parse('(let ([x 1]) [x])')
    expect(v.errors).toEqual([])
  })

  test('map literals parse to alternating key/value pairs (#334)', () => {
    const { prog, errors } = parse('(display {"a" 1 "b" (+ 1 1)})')
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe('display')
    if (stmt.tag !== 'display') return
    expect(stmt.value.tag).toBe('obj')
    if (stmt.value.tag !== 'obj') return
    expect(stmt.value.pairs.length).toBe(2)
    expect(stmt.value.pairs.map((p) => p.key.tag)).toEqual(['lit', 'lit'])
    expect(stmt.value.pairs.map((p) => p.value.tag)).toEqual(['lit', 'app'])

    expectParses('{}')
    expectParses('{"nested" {"a" [1 2]}}')
  })

  test('a map literal with an odd number of elements is an error (#334)', () => {
    const { errors } = parse('{"a" 1 "b"}')
    expect(errors.length).toBe(1)
    expect(errors[0].message).toMatch(/even number of expressions/)
  })

  test('match with number/string/char/vector/ctor/wildcard patterns', () => {
    expectParses(
      '(match x [0 "zero"] [1.5 "half"] [#\\a "a"] ["s" "str"] [#t "t"] [(cons a b) a] [[1 2] "vec"] [_ "other"])',
    )
  })

  test("wildcard pattern produces pwild, not pvar, so repeated _ doesn't collide", () => {
    const { prog, errors } = parse('(match x [_ 1] [_ 2])')
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe('stmtexp')
    if (stmt.tag !== 'stmtexp') return
    expect(stmt.expr.tag).toBe('match')
    if (stmt.expr.tag !== 'match') return
    expect(stmt.expr.branches.map((b) => b.pat.tag)).toEqual(['pwild', 'pwild'])
  })

  test('cond with test/body pairs, including zero and many branches', () => {
    expectParses('(cond)')
    expectParses('(cond [(> x 0) "pos"] [(< x 0) "neg"] [#t "zero"])')
  })

  test('struct', () => {
    expectParses('(struct point (x y))')
    expectParses('(struct empty ())')
  })

  test('import/define/display, including empty top-level list', () => {
    expectParses('(import lists)\n(define f (lambda (x) x))\n(display (f 1))')
    expectParses('()')
  })

  test('numbers', () => {
    expectParses('(display (list 42 -3.14 .5 2. 6.02e23 -1e-10 +7 0))')
  })

  test('identifiers with special characters', () => {
    expectParses('(define null? (lambda (x) (= x null)))')
    expectParses('(display (> 1 2))')
    expectParses('(display (<= 1 2))')
    expectParses('(display (set!-like-name 1))')
  })

  test('null literal', () => {
    expectParses('(display null)')
    expectParses('(match null [null "n"] [_ "other"])')
  })

  test('nested application and strings with escapes', () => {
    const { prog, errors } = parse('(display "line1\\nline2\\ttabbed")')
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe('display')
    if (stmt.tag !== 'display') return
    expect(stmt.value).toEqual(
      A.mkLit('line1\nline2\ttabbed', stmt.value.range),
    )
    expectParses('(display ((lambda (x) (x 1)) (lambda (y) y)))')
  })

  // N.B., reserved-word misuse (e.g. "(define and 5)") makes the Lezer parser
  // emit an error-recovery node rather than a clean tree, since kw<> keywords
  // are @specialize'd and can never be re-read as plain identifiers -- this
  // is exercised in errors.test.ts instead.

  test("define's preceding doc comments are captured, unparsed", () => {
    // N.B., the bridge only captures the raw comments -- parsing them into a
    // FunctionDoc is deferred (see docstring.ts's parseFunctionDocFromComments),
    // so malformed docstrings can never fail this parse. Parsing on demand is
    // exercised directly in docstring.test.ts.
    const src = [
      ';;; (add1 x) -> number?',
      ';;;  x : number?',
      ';;; Adds one to a number.',
      '(define add1 (lambda (x) (+ x 1)))',
    ].join('\n')
    const { prog, errors } = parse(src)
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe('define')
    if (stmt.tag !== 'define') return
    expect(stmt.docComments?.map((c) => c.line)).toEqual([
      ';;; (add1 x) -> number?',
      ';;;  x : number?',
      ';;; Adds one to a number.',
    ])
  })

  test('inline line comments are skipped anywhere, not just at top level (#302)', () => {
    // N.B., LineComment is a @skip token, so Lezer can attach one between any
    // two children. Regression: comments inside a form used to reach the AST
    // builder and throw "Unexpected expression node: LineComment".
    expectParses("(+ 1 ; this shouldn't be an error\n   1)")
    expectParses('(+ 1\n ; own-line comment\n 1)')
    expectParses('(define x ; the value\n 5)')
    expectParses('(let ([x 1] ; binding\n [y 2]) (+ x y))')
    expectParses('(cond [(> 1 0) ; yes\n 1] [else 2])')
    expectParses('(list 1 2 ; trailing before close paren\n)')
  })

  test('an inline comment does not shift positional parsing (#302)', () => {
    // The comment between 1 and 2 must not become a phantom argument.
    const { prog, errors } = parse('(+ 1 ; note\n 2)')
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe('stmtexp')
    if (stmt.tag !== 'stmtexp') return
    expect(stmt.expr.tag).toBe('app')
    if (stmt.expr.tag !== 'app') return
    expect(stmt.expr.args.length).toBe(2)
  })

  test('an inline comment inside a vector or map literal is dropped, not turned into a phantom element (#302)', () => {
    const v = parse('(display [1 2 ; note\n 3])')
    expect(v.errors).toEqual([])
    const vStmt = v.prog[0]
    expect(vStmt.tag).toBe('display')
    if (vStmt.tag !== 'display' || vStmt.value.tag !== 'vec') return
    expect(vStmt.value.exps.length).toBe(3)

    // A comment must not count toward the map literal's element parity either.
    const m = parse('(display {"a" 1 ; note\n "b" 2})')
    expect(m.errors).toEqual([])
    const mStmt = m.prog[0]
    expect(mStmt.tag).toBe('display')
    if (mStmt.tag !== 'display' || mStmt.value.tag !== 'obj') return
    expect(mStmt.value.pairs.length).toBe(2)
  })

  test("a define's docstring is still captured when its body has an inline comment (#302)", () => {
    const src = [
      ';;; (add1 x) -> number?',
      '(define add1 (lambda (x) ; add one\n (+ x 1)))',
    ].join('\n')
    const { prog, errors } = parse(src)
    expect(errors).toEqual([])
    const stmt = prog[0]
    expect(stmt.tag).toBe('define')
    if (stmt.tag !== 'define') return
    expect(stmt.docComments?.map((c) => c.line)).toEqual([
      ';;; (add1 x) -> number?',
    ])
  })

  test('every reserved word is exercised by at least one sample above', () => {
    // N.B., a lightweight guard against silently losing coverage of a form
    // as reservedWords grows.
    expect(reservedWords.slice().sort()).toEqual(
      [
        'and',
        'begin',
        'cond',
        'define',
        'define-export',
        'export',
        'display',
        'if',
        'import',
        'lambda',
        'let',
        'match',
        'or',
        'struct',
      ].sort(),
    )
  })
})
