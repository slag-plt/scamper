import { expect, test } from 'vitest'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import type { ScamperDiagnostic } from '../../src/scheme/diagnostic'
import type { Comment, Stmt } from '../../src/scheme/ast'

// A docstring attaches to the definition *directly* above it. precedingComments
// used to walk back over blank lines, so a `;;;` file header ran into the first
// definition's docstring -- and since the docstring parser drops non-`;;;`
// lines and joins the rest, the result parsed as malformed. The definition then
// lost its documentation *and* its contract, with no error anywhere: the IDE
// reports the warning for a user's own file, but the standard library's load
// path throws it away (see test/libs/docstrings.test.ts).

/** The doc comments the parser attached to the nth define in `src`. */
function docCommentsOf(src: string, n = 0): Comment[] {
  const errs: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(errs, src)
  expect(errs, 'test source should parse cleanly').toEqual([])
  const defines = prog.filter(
    (s: Stmt) => s.tag === 'define' || s.tag === 'defexport',
  )
  return (defines[n] as { docComments?: Comment[] }).docComments ?? []
}

/** True iff the nth define's docstring parses into a FunctionDoc. */
function docParses(src: string, n = 0): boolean {
  return parseFunctionDocFromComments(docCommentsOf(src, n)).doc !== undefined
}

const DOCSTRING = [
  ';;; (f x) -> number?',
  ';;;  x : number?',
  ';;; Returns x.',
  '(define f (lambda (x) x))',
].join('\n')

test('a blank line separates a file header from the first docstring', () => {
  const src = [';;; A module header.', ';;; Second line.', '', DOCSTRING].join('\n')
  expect(docCommentsOf(src).map((c) => c.line)).toEqual([
    ';;; (f x) -> number?',
    ';;;  x : number?',
    ';;; Returns x.',
  ])
  expect(docParses(src)).toBe(true)
})

test('a blank line separates one definition\'s comments from the next', () => {
  const src = [
    ';;; (g) -> number?',
    ';;; Returns one.',
    '(define g (lambda () 1))',
    '',
    DOCSTRING,
  ].join('\n')
  expect(docCommentsOf(src, 1).map((c) => c.line)).toEqual([
    ';;; (f x) -> number?',
    ';;;  x : number?',
    ';;; Returns x.',
  ])
  expect(docParses(src, 1)).toBe(true)
})

test('a comment trailing a line of code is not the next definition\'s docstring', () => {
  const src = ['(define a 1) ;;; not a docstring', DOCSTRING].join('\n')
  expect(docCommentsOf(src, 1).map((c) => c.line)).toEqual([
    ';;; (f x) -> number?',
    ';;;  x : number?',
    ';;; Returns x.',
  ])
  expect(docParses(src, 1)).toBe(true)
})

test('an ordinary ;; comment directly above a docstring is still harmless', () => {
  const src = [';; A note for the reader.', DOCSTRING].join('\n')
  // It is attached -- the block is contiguous -- and the docstring parser drops
  // it, as it drops every line that is not `;;;`.
  expect(docCommentsOf(src)).toHaveLength(4)
  expect(docParses(src)).toBe(true)
})

// The remaining sharp edge, left as-is: with no blank line, the whole run is
// genuinely one block, and the `;;` line is dropped rather than treated as a
// separator -- so the two `;;;` runs are joined. Writing a blank line is the fix.
test('a ;; line does not separate two ;;; runs inside one contiguous block', () => {
  const src = [';;; A module header.', ';; an aside', DOCSTRING].join('\n')
  expect(docParses(src)).toBe(false)
})
