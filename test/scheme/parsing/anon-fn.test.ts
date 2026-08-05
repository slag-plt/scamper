import { describe, expect, test } from 'vitest'
import { parse, expectParses } from './test-utils'
import * as A from '../../../src/scheme/ast'

/** Parse `src` and return only the (message-bearing) diagnostics. */
function errorsFor(src: string): string[] {
  return parse(src).errors.map((d) => d.message)
}

/** Parse a single bare-expression statement and return its expression. */
function parseExp(src: string): A.Exp {
  const { prog, errors } = parse(src)
  expect(errors, `unexpected parse errors for ${JSON.stringify(src)}`).toEqual(
    [],
  )
  const stmt = prog[0]
  if (!A.isStmtExp(stmt)) throw new Error('expected a bare expression')
  return stmt.expr
}

describe('anonymous function parsing', () => {
  test('well-formed #(...) forms parse', () => {
    expectParses('#(+ %1 1)')
    expectParses('#(+ % 1)')
    expectParses('#(list %1 %2 %3)')
    expectParses('#(apply + %&)')
    expectParses('#(cons %1 %&)')
    expectParses('#(f)')
    expectParses('#()')
    expectParses('(map #(* %1 %1) (list 1 2 3))')
    // The body is an ordinary parenthesized expression, so any form -- not just
    // an application -- may be the body, including the special forms.
    expectParses('#(if % 1 2)')
    expectParses('#(let ([x %1]) (+ x %2))')
    expectParses('#(and % %2)')
    expectParses('#(or % %2)')
    expectParses('#(begin (println %1) %2)')
    expectParses('#(cond [% 1] [#t 2])')
    expectParses('#(match % [0 "zero"] [_ "other"])')
    expectParses('#(map (lambda (x) (+ x %1)) %&)')
    // The brace form of a paren works too.
    expectParses('#{+ %1 1}')
  })

  test('#(...) parses to an anonfn node wrapping the body expression', () => {
    const e = parseExp('#(+ %1 1)')
    expect(e.tag).toBe('anonfn')
    if (e.tag !== 'anonfn') return
    // The body is the parenthesized expression verbatim (here an application).
    expect(e.body.tag).toBe('app')
    expect(A.expToString(e)).toBe('#(+ %1 1)')
  })

  test('a special form may be the body', () => {
    const e = parseExp('#(if % 1 2)')
    expect(e.tag).toBe('anonfn')
    if (e.tag !== 'anonfn') return
    expect(e.body.tag).toBe('if')
    expect(A.expToString(e)).toBe('#(if % 1 2)')
  })

  test('the body and operand ranges span the right source text', () => {
    const src = '#(+ %1 1)'
    const e = parseExp(src)
    if (e.tag !== 'anonfn' || e.body.tag !== 'app') throw new Error('bad parse')
    const slice = (r: { begin: { idx: number }; end: { idx: number } }) =>
      src.slice(r.begin.idx, r.end.idx + 1)
    expect(slice(e.range)).toBe('#(+ %1 1)')
    expect(slice(e.body.range)).toBe('(+ %1 1)')
    expect(slice(e.body.head.range)).toBe('+')
    expect(slice(e.body.args[0].range)).toBe('%1')
  })
})

describe('anonymous function restrictions', () => {
  test('a % identifier outside #(...) is rejected', () => {
    expect(errorsFor('(+ % 1)')).toContainEqual(
      expect.stringContaining('can only be used inside an anonymous function'),
    )
    expect(errorsFor('(+ %1 1)')).toContainEqual(
      expect.stringContaining('can only be used inside an anonymous function'),
    )
    expect(errorsFor('%&')).toContainEqual(
      expect.stringContaining('can only be used inside an anonymous function'),
    )
  })

  test('a % identifier is rejected in a binder position outside #(...)', () => {
    expect(errorsFor('(define %1 5)').length).toBeGreaterThan(0)
    expect(errorsFor('(lambda (%1) %1)').length).toBeGreaterThan(0)
  })

  test('nested #(...) is rejected', () => {
    expect(errorsFor('#(f #(g %1))')).toContainEqual(
      expect.stringContaining('cannot be nested'),
    )
    expect(errorsFor('#(#(%1))')).toContainEqual(
      expect.stringContaining('cannot be nested'),
    )
  })

  test('an identifier that starts with % but is not a valid % identifier is rejected', () => {
    for (const src of [
      '#(+ %foo 1)',
      '#(+ %0 1)',
      '#(+ %01 1)',
      '#(+ %1a 1)',
      // "%&"-prefixed junk tokenizes as one identifier (not "%&" + rest), so it
      // is caught by the same rule rather than slipping through as two params.
      '#(f %&x)',
      '#(f %&%)',
    ]) {
      expect(errorsFor(src), src).toContainEqual(
        expect.stringContaining('identifiers cannot begin with "%"'),
      )
    }
  })

  test('the invalid-% rule holds outside #(...) too', () => {
    expect(errorsFor('(+ %foo 1)')).toContainEqual(
      expect.stringContaining('identifiers cannot begin with "%"'),
    )
    expect(errorsFor('(define %x 5)')).toContainEqual(
      expect.stringContaining('identifiers cannot begin with "%"'),
    )
  })

  test('valid % identifiers inside #(...) are accepted', () => {
    expect(errorsFor('#(f % %1 %2 %10 %&)')).toEqual([])
  })

  test('a % identifier is rejected as a binder even inside #(...)', () => {
    // % identifiers are the implicit parameters; binding one (which would
    // shadow the parameter it names) is rejected wherever binders appear.
    for (const src of [
      '#((lambda (%1) %1) %2)', // lambda parameter
      '#(f (let ([%1 5]) %1))', // let binder
      '#(f (match %1 [%2 x]))', // match pattern variable
    ]) {
      expect(errorsFor(src), src).toContainEqual(
        expect.stringContaining('cannot be used as a binding name'),
      )
    }
  })

  test('quoted #(...) is raw list data with no spurious null', () => {
    // #(...) as inert data is just the list of its operands (finding: the "#("
    // token must not leak in as an empty-list null).
    expect(A.expToString(parseExp("'#(f x)"))).toBe(
      A.expToString(parseExp("'(f x)")),
    )
    expect(A.expToString(parseExp("'#(f x)"))).not.toContain('null')
  })
})
