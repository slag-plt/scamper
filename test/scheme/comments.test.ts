import { describe, expect, test } from 'vitest'
import { tokenizeAndParse } from '../../src/scheme'
import * as A from '../../src/scheme/ast'
import { attachComments, collectComments } from '../../src/scheme/comments'

function lines(cs: A.Comment[] | undefined): string[] {
  return (cs ?? []).map((c) => c.line)
}

function attach(src: string): A.ProgNode {
  const { program, diagnostics } = tokenizeAndParse(src)
  if (!program) throw new Error(diagnostics.map((d) => d.message).join('; '))
  const root = A.progToNode(program)
  attachComments(root, collectComments(src))
  return root
}

describe('collectComments', () => {
  test('recovers line comments in source order with their text', () => {
    expect(lines(collectComments('; hello\n(+ 1 2) ; world'))).toEqual([
      '; hello',
      '; world',
    ])
  })

  test('ignores ";" inside strings and char literals', () => {
    expect(collectComments('(define s "a;b")\n(define c #\\;)')).toEqual([])
  })

  test('trims trailing whitespace from a comment', () => {
    expect(lines(collectComments('(+ 1 2)   ;  spaced   '))).toEqual([
      ';  spaced',
    ])
  })
})

describe('attachComments — placement rules', () => {
  test('a standalone comment leads the following statement', () => {
    const define = attach('; note\n(define x 1)').body[0]
    expect(lines(define.leading)).toEqual(['; note'])
    expect(lines(define.trailing)).toEqual([])
  })

  test('a same-line comment trails the preceding statement', () => {
    const define = attach('(define x 1) ; trailing').body[0]
    expect(lines(define.trailing)).toEqual(['; trailing'])
    expect(lines(define.leading)).toEqual([])
  })

  test('a mid-form comment trails the element on its line', () => {
    const stmt = attach('(+ 1 ; one\n 2)').body[0]
    if (stmt.tag !== 'stmtexp' || stmt.expr.tag !== 'app') throw new Error('shape')
    expect(lines(stmt.expr.args[0].trailing)).toEqual(['; one'])
    expect(lines(stmt.expr.args[1].leading)).toEqual([])
  })

  test('an own-line comment before an element leads that element', () => {
    const stmt = attach('(+ 1\n ; note\n 2)').body[0]
    if (stmt.tag !== 'stmtexp' || stmt.expr.tag !== 'app') throw new Error('shape')
    expect(lines(stmt.expr.args[0].trailing)).toEqual([])
    expect(lines(stmt.expr.args[1].leading)).toEqual(['; note'])
  })

  test('a comment before a closing paren dangles on the form', () => {
    const stmt = attach('(+ 1 2\n ; note\n)').body[0]
    if (stmt.tag !== 'stmtexp' || stmt.expr.tag !== 'app') throw new Error('shape')
    expect(lines(stmt.expr.dangling)).toEqual(['; note'])
  })

  test('a comment-only program dangles on the root', () => {
    expect(lines(attach('; only').dangling)).toEqual(['; only'])
  })

  test('a comment attaches to a nested identifier (lambda parameter)', () => {
    const stmt = attach('(lambda (x ; the x\n y) x)').body[0]
    if (stmt.tag !== 'stmtexp' || stmt.expr.tag !== 'lam') throw new Error('shape')
    // The comment is on the same line as `x`, so it trails the `x` parameter.
    expect(lines(stmt.expr.params[0].trailing)).toEqual(['; the x'])
  })

  test('trailing vs. leading is decided per line, never double-attached', () => {
    // "; a" trails `1`; "; b" (own line) leads `2`.
    const stmt = attach('(+ 1 ; a\n ; b\n 2)').body[0]
    if (stmt.tag !== 'stmtexp' || stmt.expr.tag !== 'app') throw new Error('shape')
    expect(lines(stmt.expr.args[0].trailing)).toEqual(['; a'])
    expect(lines(stmt.expr.args[1].leading)).toEqual(['; b'])
  })

  test('a define docstring becomes the leading comments of its statement', () => {
    const define = attach(';;; (f x) -> number?\n;;; doc\n(define f (lambda (x) x))').body[0]
    expect(lines(define.leading)).toEqual([';;; (f x) -> number?', ';;; doc'])
  })
})
