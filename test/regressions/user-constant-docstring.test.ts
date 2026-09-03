import { describe, expect, test } from 'vitest'
import { compile } from '../../src/scheme/index'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// #450: a constant is documented `name: predicate` (#412), and the standard
// library writes them that way. A *user's* file goes through a pass the
// library's load path never runs -- scope.ts's scopeCheckFunctionDoc, which
// the IDE's live linter drives -- and that pass assumed every docstring
// documented a lambda. So every documented constant in a student's own file
// was underlined with "Function docstring attached to non-function
// definition", with no way to write one that did not warn.

/** The "Docstring" warnings the scope-check pass reports for `src`. */
async function docstringWarnings(src: string): Promise<string[]> {
  const { diagnostics } = await compile(src, { scopeCheck: true })
  return diagnostics
    .filter((d: ScamperDiagnostic) => d.phase === 'Docstring')
    .map((d: ScamperDiagnostic) => d.message)
}

describe('a user can document a constant in their own file (#450)', () => {
  test.each([
    ['the colon written tight', ';;; my-pi: number?'],
    ['the colon written spaced', ';;; my-pi : number?'],
  ])('%s', async (_label, sigLine) => {
    const src = [
      sigLine,
      ";;; The ratio of a circle's circumference to its diameter.",
      '(define my-pi 3.14159)',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([])
  })

  test('define-export documents a constant just as well', async () => {
    const src = [
      ';;; my-pi: number?',
      ';;; The ratio.',
      '(define-export my-pi 3.14159)',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([])
  })

  test('the documented name must still be the defined one', async () => {
    const src = [
      ';;; my-pi: number?',
      ';;; The ratio.',
      '(define my-tau 6.28318)',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([
      'Docstring name "my-pi" does not match defined name "my-tau"',
    ])
  })

  test("a constant's predicate is still scope-checked", async () => {
    const src = [
      ';;; my-pi: nomber?',
      ';;; The ratio.',
      '(define my-pi 3.14159)',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual(['Undefined predicate "nomber?"'])
  })

  test('a function docstring on a non-function is still a mistake', async () => {
    const src = [
      ';;; (my-pi) -> number?',
      ';;; The ratio.',
      '(define my-pi 3.14159)',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([
      'Function docstring attached to non-function definition. A constant is documented "name: predicate".',
    ])
  })

  // A constant docstring over a lambda is a truthful description of a value
  // that happens to be a procedure, so it is deliberately left unwarned.
  test('a lambda may be described as the value it is', async () => {
    const src = [
      ';;; f: procedure?',
      ';;; The identity function.',
      '(define f (lambda (x) x))',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([])
  })

  test('an ordinary documented function is unaffected', async () => {
    const src = [
      ';;; (f x) -> number?',
      ';;;   x : number?',
      ';;; Returns x.',
      '(define f (lambda (x) x))',
    ].join('\n')
    expect(await docstringWarnings(src)).toEqual([])
  })
})
