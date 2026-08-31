import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import { functionDocSignature } from '../../src/scheme/docstring/render'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import { Comment } from '../../src/scheme/ast'
import { Range } from '../../src/lpm'

const comments = (src: string): Comment[] =>
  src
    .trim()
    .split('\n')
    .map((line) => ({ line: line.trim(), range: Range.none }))

// https://github.com/slag-plt/scamper/issues/435
//
// A docstring could declare required parameters and a rest parameter but
// nothing in between, so every documented function had a fixed arity and
// `(substring "alphabetical" 5)` -- legal in Racket, and the traditional way
// to say "from here to the end" -- was an arity error. A signature can now
// mark a parameter optional by bracketing it, `(substring s start [end])`,
// and an optional argument that is not supplied arrives as void.

test('substring takes its end index optionally (#435)', async () => {
  expect(
    await runProgram(`
  (substring "alphabetical" 5)
  (substring "alphabetical" 5 8)
  (substring "alphabetical" 0)
  `),
  ).toEqual(['"betical"', '"bet"', '"alphabetical"'])
})

test('an optional argument is still checked against its predicate (#435)', async () => {
  expect(
    await runProgram(
      `
  (substring "alphabetical" 5 "x")
  (substring "alphabetical" 5 8 9)
  `,
      { stripRanges: true },
    ),
  ).toEqual([
    'Runtime error: (error) expected an integer, received string',
    'Runtime error: (substring) Arity mismatch in function call: expected at most 3 arguments, got 4',
  ])
})

// Contracts are inserted for the standard library only (see src/lib/index.ts),
// so these exercise the generated wrapper the way the library gets it.
test('a documented definition can declare optional parameters (#435)', async () => {
  expect(
    await runProgram(
      `
  ;;; (greet name [greeting]) -> string?
  ;;;  name : string?
  ;;;  greeting : string?
  ;;;   defaults to "Hello"
  ;;; Greets someone.
  (define greet
    (lambda (name greeting)
      (string-append (if (void? greeting) "Hello" greeting) ", " name)))
  (greet "Ada")
  (greet "Ada" "Howdy")
  (greet "Ada" 3)
  (greet)
  `,
      { insertContracts: true, stripRanges: true },
    ),
  ).toEqual([
    '"Hello, Ada"',
    '"Howdy, Ada"',
    'Runtime error: (error) expected a string, received number',
    'Runtime error: Arity mismatch in function call: expected 1 arguments, got 0',
  ])
})

test('optional parameters may sit before a rest parameter (#435)', async () => {
  expect(
    await runProgram(
      `
  ;;; (tally [scale] & xs) -> number?
  ;;;  scale : number?
  ;;;   defaults to 1
  ;;;  xs : number?
  ;;; Adds up xs, scaled.
  (define tally
    (lambda (scale & xs)
      (* (if (void? scale) 1 scale) (fold + 0 xs))))
  (tally)
  (tally 2)
  (tally 2 1 2 3)
  (tally 2 1 "x")
  `,
      { insertContracts: true, stripRanges: true },
    ),
  ).toEqual([
    '0',
    '0',
    '12',
    'Runtime error: (error) expected every value of xs to be a number, but at least one was not',
  ])
})

test('a signature renders its optional parameters bracketed (#435)', () => {
  const { doc, diagnostics } = parseFunctionDocFromComments(
    comments(`
    ;;; (substring s start [end]) -> string?
    ;;;  s : string?
    ;;;  start : integer?
    ;;;  end : integer?
    ;;;   defaults to the end of \`s\`
    ;;; Returns part of a string.
  `),
  )
  expect(diagnostics).toEqual([])
  if (doc === undefined) {
    throw new Error('the docstring did not parse')
  }
  expect(functionDocSignature(doc)).toBe(
    [
      '(substring s start [end]) -> string?',
      '  s: string?',
      '  start: integer?',
      '  end: integer?',
      '    defaults to the end of `s`',
    ].join('\n'),
  )
})

test('a required parameter cannot follow an optional one (#435)', () => {
  const { doc, diagnostics } = parseFunctionDocFromComments(
    comments(`
    ;;; (f a [b] c) -> any
    ;;;  a : any
    ;;;  b : any
    ;;;  c : any
    ;;; Does something.
  `),
  )
  expect(doc).toBeUndefined()
  expect(diagnostics[0].message).toContain(
    'a required parameter cannot follow an optional one',
  )
})

test.each([
  ['a required parameter after an optional one', '(f a [b] c)', 'a required parameter cannot follow an optional one'],
  ['a doubly bracketed name', '(f [[b]])', 'an optional parameter is written as a name in one pair of brackets'],
  ['a bracketed rest parameter', '(f a & [xs])', 'an optional parameter is written as a name in one pair of brackets'],
  ['a bracketed rest marker', '(f a [&])', 'an optional parameter is written as a name in one pair of brackets'],
])('rejects %s (#435)', (_what, signature, message) => {
  const names = ['a', 'b', 'c', 'xs']
  const { doc, diagnostics } = parseFunctionDocFromComments(
    comments(
      [
        `;;; ${signature} -> any`,
        ...names.map((n) => `;;;  ${n} : any`),
        ';;; Does something.',
      ].join('\n'),
    ),
  )
  expect(doc).toBeUndefined()
  expect(diagnostics[0].message).toContain(message)
})

test('a misspelled parameter line is caught rather than costing the optional (#435)', () => {
  const { doc, diagnostics } = parseFunctionDocFromComments(
    comments(`
    ;;; (sub s start [end]) -> string?
    ;;;  s : string?
    ;;;  start : integer?
    ;;;  edn : integer?
    ;;; Returns part of a string.
  `),
  )
  expect(doc).toBeUndefined()
  expect(diagnostics[0].message).toContain(
    'Parameter "edn" is not declared in the signature',
  )
})
