import { describe, expect, test } from 'vitest'
import { parse } from './test-utils'
import * as A from '../../../src/scheme/ast'

// Qualified names (`mod.member`) and the qualified import form `(import m alias)`.
// The grammar tokenizes a one-level dotted name as a single Identifier; whether
// it's legal in a given position (a reference, never a binder) is decided in
// lezer-bridge.ts. See src/scheme/syntax.grammar and lezer-bridge.ts.

// The single expression of a one-statement program.
function soleExp(src: string): A.Exp {
  const { prog, errors } = parse(src)
  expect(errors, `unexpected parse errors for ${JSON.stringify(src)}`).toEqual([])
  expect(prog.length).toBe(1)
  expect(prog[0].tag).toBe('stmtexp')
  return (prog[0] as A.StmtExp).expr
}

describe('qualified identifiers', () => {
  test('a bare qualified reference is one identifier carrying the dotted name', () => {
    const e = soleExp('img.outlined-square')
    expect(e.tag).toBe('id')
    expect((e as A.Identifier).name).toBe('img.outlined-square')
  })

  test('a qualified name works as the head of an application', () => {
    const e = soleExp('(img.square 5)')
    expect(e.tag).toBe('app')
    const head = (e as A.App).head
    expect(head.tag).toBe('id')
    expect((head as A.Identifier).name).toBe('img.square')
  })

  test('a qualified name works as an argument', () => {
    const e = soleExp('(+ img.a m.b)')
    const args = (e as A.App).args.map((a) => (a as A.Identifier).name)
    expect(args).toEqual(['img.a', 'm.b'])
  })

  test('splitQualifiedName round-trips the two halves', () => {
    expect(A.isQualifiedName('img.square')).toBe(true)
    expect(A.isQualifiedName('square')).toBe(false)
    expect(A.splitQualifiedName('img.square')).toEqual({
      qualifier: 'img',
      member: 'square',
    })
  })
})

describe('qualified identifiers are rejected outside reference position', () => {
  // A dotted name in any binder/pattern slot is a parse error.
  test.each([
    ['a define name', '(define a.b 5)'],
    ['a lambda parameter', '(lambda (a.b) a.b)'],
    ['a let binder', '(let ([a.b 1]) a.b)'],
    ['a struct name', '(struct a.b (x))'],
    ['a struct field', '(struct s (a.b))'],
  ])('%s', (_name, src) => {
    const { errors } = parse(src)
    expect(errors.length).toBeGreaterThan(0)
  })
})

describe('malformed qualified names', () => {
  test('two levels of qualification (a.b.c) does not tokenize as one name', () => {
    const { errors } = parse('a.b.c')
    expect(errors.length).toBeGreaterThan(0)
  })

  test('a trailing dot is a parse error', () => {
    const { errors } = parse('foo.')
    expect(errors.length).toBeGreaterThan(0)
  })

  test('a reserved word as a half is rejected', () => {
    const { errors } = parse('if.x')
    expect(errors.length).toBeGreaterThan(0)
  })
})

describe('qualified imports', () => {
  test('(import m alias) records the alias on a builtin import', () => {
    const { prog, errors } = parse('(import image img)')
    expect(errors).toEqual([])
    const s = prog[0] as A.Import
    expect(s.tag).toBe('import')
    expect(s.module).toBe('image')
    expect(s.kind).toBe('builtin')
    expect(s.alias).toBe('img')
  })

  test('(import "f.scm" alias) records the alias on a file import', () => {
    const { prog, errors } = parse('(import "utils.scm" u)')
    expect(errors).toEqual([])
    const s = prog[0] as A.Import
    expect(s.kind).toBe('file')
    expect(s.module).toBe('utils.scm')
    expect(s.alias).toBe('u')
  })

  test('the one-argument import form leaves the alias unset', () => {
    const { prog } = parse('(import image)')
    expect((prog[0] as A.Import).alias).toBeUndefined()
  })

  test('a qualified (dotted) alias is rejected', () => {
    const { errors } = parse('(import image a.b)')
    expect(errors.length).toBeGreaterThan(0)
  })

  test('a reserved word as an alias is rejected', () => {
    const { errors } = parse('(import image if)')
    expect(errors.length).toBeGreaterThan(0)
  })
})
