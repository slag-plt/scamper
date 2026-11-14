import { describe, expect, test } from 'vitest'
import { compile } from '../../src/scheme/index'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// https://github.com/slag-plt/scamper/issues/745
//
// The check behind "Function docstring attached to non-function definition" was
// `!A.isLam(value)` (scope.ts), so it held that a function is a literal lambda
// and nothing else. Every other way of producing one was reported as a mistake:
// a `let` wrapping the lambda -- the shape in the issue, and the ordinary way
// to give a function private helpers -- but also `(o f g)`, `(if c f g)`,
// `(l-s + 5)` and `(define inc add1)`.
//
// That is the worst kind of warning: a student who tries to clear every warning
// cannot clear this one, because the code is right. The judgement that replaced
// it, and why it is three-valued, is documented in src/scheme/function-shape.ts;
// these are its end-to-end consequences. The first describe block is what the
// issue asked for, the second is what keeps the warning worth having, and the
// third is the half of the old check nobody noticed: it did not merely warn, it
// returned, so a let-wrapped function's docstring went unchecked entirely.

/** The "Docstring" warnings the scope-check pass reports for `src`. */
async function docstringWarnings(src: string): Promise<string[]> {
  const { diagnostics } = await compile(src, { scopeCheck: true })
  return diagnostics
    .filter((d: ScamperDiagnostic) => d.phase === 'Docstring')
    .map((d: ScamperDiagnostic) => d.message)
}

describe('a documented function need not be a bare lambda (#745)', () => {
  test("the issue's own definition, with its parameter named as documented", async () => {
    // Rebelsky's example, with `strings` in the lambda to match the docstring.
    // The warning it was filed about is gone; see the test below for what
    // happens when the two names disagree, as they did in the issue.
    const src = [
      ';;; (longest-string strings) -> string?',
      ';;;   strings : (list-of string?)',
      ';;; Find the longest string in a nonempty list of strings.',
      '(define longest-string',
      '  (let ([longer (lambda (str1 str2)',
      '                  (if (>= (string-length str1) (string-length str2))',
      '                      str1',
      '                      str2))])',
      '    (lambda (strings)',
      '      (fold longer (car strings) (cdr strings)))))',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([])
  })

  test.each([
    ['a let around the lambda', '(define f (let ([g (lambda (y) y)]) (lambda (x) x)))'],
    ['a begin ending in the lambda', '(define f (begin 1 (lambda (x) x)))'],
    ['an if between two lambdas', '(define f (if #t (lambda (x) x) (lambda (x) 1)))'],
    ['a cond between two lambdas', '(define f (cond [#t (lambda (x) x)] [else (lambda (x) 1)]))'],
    ['a composition with o', '(define f (o add1 add1))'],
    ['a composition with compose', '(define f (compose add1 add1))'],
    ['a left section', '(define f (l-s + 5))'],
    ['a right section', '(define f (r-s + 5))'],
    ['another function by name', '(define f add1)'],
    ['a value whose shape is unknown', '(define f (vector-ref (vector add1) 0))'],
    ['a hole, i.e. not written yet', '(define f ??)'],
    // A pattern variable spelled like a library name is the pattern's, not
    // the library's; read the other way round these warned.
    ['a function destructured out of a pair', '(define f (match (pair add1 1) [(pair pi n) pi]))'],
    ['a function destructured by a let', '(define f (let ([(pair pi n) (pair add1 1)]) pi))'],
    // One branch a lambda, the other a function by another route.
    ['an if between a lambda and a named function', '(define f (if #t (lambda (x) x) add1))'],
  ])('%s does not warn', async (_label, definition) => {
    const src = [
      ';;; (f x) -> number?',
      ';;;   x : number?',
      ';;; Adds one, somehow.',
      definition,
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([])
  })
})

describe('the warning still fires for a real non-function (#745)', () => {
  test.each([
    ['a number', '(define f 3.14159)'],
    ['a string', '(define f "hi")'],
    ['arithmetic', '(define f (+ 1 2))'],
    ['a list', '(define f (list 1 2))'],
    ['a vector', '(define f (vector 1 2))'],
    ['a let around a literal', '(define f (let ([g (lambda (x) x)]) 5))'],
  ])('%s warns', async (_label, definition) => {
    const src = [';;; (f) -> number?', ';;; A number.', definition].join('\n')
    expect(await docstringWarnings(src)).toEqual([
      'Function docstring attached to non-function definition. A constant is documented "name: predicate".',
    ])
  })

  test('as it does for a name bound to a non-function', async () => {
    const src = [
      ';;; my-pi: number?',
      ";;; The ratio of a circle's circumference to its diameter.",
      '(define my-pi 3.14159)',
      ';;; (f) -> number?',
      ';;; An alias of a constant.',
      '(define f my-pi)',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([
      'Function docstring attached to non-function definition. A constant is documented "name: predicate".',
    ])
  })
})

// The second half of #745: the old check did not merely warn, it `return`ed,
// so a let-wrapped function's docstring was never checked against anything.
// Recovering the lambda puts those checks back.
describe('a recovered lambda is still checked against its docstring (#745)', () => {
  test('a parameter the docstring names differently is reported', async () => {
    // This is the issue's example as actually written -- documented
    // `(longest-string strings)` over a lambda taking `lst`. It no longer
    // warns that the definition is not a function; it warns that the docstring
    // and the lambda disagree, which is true and was previously missed.
    const src = [
      ';;; (f strings) -> string?',
      ';;;   strings : (list-of string?)',
      ';;; Takes a list.',
      '(define f (let ([g (lambda (y) y)]) (lambda (lst) (car lst))))',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([
      'Function signature defines parameter "lst" in this position but docstring signature instead defines "strings"',
      'Docstring describes unknown function parameter "strings"',
      'Description of function parameter "lst" missing',
    ])
  })

  test('a predicate typo is reported, which the early return used to hide', async () => {
    const src = [
      ';;; (f lst) -> string?',
      ';;;   lst : (list-of strnig?)',
      ';;; Takes a list.',
      '(define f (let ([g (lambda (y) y)]) (lambda (lst) (car lst))))',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([
      'Undefined predicate "strnig?"',
    ])
  })

  test('and is reported even when no lambda can be recovered', async () => {
    // `(o add1 add1)` is a function, but there is no lambda to read parameters
    // off, so only the predicates can be checked -- and they are.
    const src = [
      ';;; (f x) -> nmber?',
      ';;;   x : number?',
      ';;; Adds two.',
      '(define f (o add1 add1))',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual(['Undefined predicate "nmber?"'])
  })

  test('nor is a lone lambda beside a branch that is a function another way', async () => {
    // `add1` takes a parameter too, and its name is as much the definition's
    // as the lambda's, so the docstring is held to neither. Only the
    // predicates are checked.
    const src = [
      ';;; (f n) -> nmber?',
      ';;;   n : number?',
      ';;; Adds one.',
      '(define f (if #t (lambda (x) x) add1))',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual(['Undefined predicate "nmber?"'])
  })

  test('two lambdas that disagree are not blamed for matching one of them', async () => {
    // Both branches are the function, so neither can be held to the docstring.
    // The predicates are still checked.
    const src = [
      ';;; (f x) -> nmber?',
      ';;;   x : number?',
      ';;; Picks one.',
      '(define f (if #t (lambda (x) x) (lambda (y) y)))',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual(['Undefined predicate "nmber?"'])
  })
})
