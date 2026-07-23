import { describe, expect, test } from 'vitest'
import { tokenizeAndParse } from '../../src/scheme'
import { SimpleErrorChannel } from '../../src/lpm/output/simple-error'

// A bare-identifier import, e.g. `(import lists)`, denotes a built-in
// library. A quoted-string import, e.g. `(import "my-file.scm")`, denotes a
// user file. These two forms are distinguished at parse time -- not, as
// before, by probing for the name's existence at runtime -- because a file
// name routinely contains a "." (its extension), and "." can never appear in
// a bare identifier (see syntax.grammar's Identifier token, which carves "."
// out to make room for the lambda rest-parameter dot). Before this was
// fixed, `(import example-defns.scm)` failed to parse at all: "." isn't part
// of Identifier, so the module name split into three tokens and Import's
// grammar production (which only ever accepted one Identifier) rejected it.

describe('import: built-in library vs. file', () => {
  test('a bare identifier import is parsed as a built-in library', () => {
    const err = new SimpleErrorChannel()
    const prog = tokenizeAndParse(err, '(import prelude)')
    expect(err.errors).toEqual([])
    expect(prog?.length).toBe(1)
    const stmt = prog?.[0]
    expect(stmt?.tag).toBe('import')
    if (stmt?.tag !== 'import') return
    expect(stmt.module).toBe('prelude')
    expect(stmt.kind).toBe('builtin')
  })

  test("a quoted file import (including one with a '.' in its name) is parsed as a file", () => {
    const err = new SimpleErrorChannel()
    const prog = tokenizeAndParse(err, '(import "example-defns.scm")')
    expect(err.errors).toEqual([])
    expect(prog?.length).toBe(1)
    const stmt = prog?.[0]
    expect(stmt?.tag).toBe('import')
    if (stmt?.tag !== 'import') return
    expect(stmt.module).toBe('example-defns.scm')
    expect(stmt.kind).toBe('file')
  })

  // A file import must be quoted -- a bare identifier can never contain ".",
  // so this remains a parse error, just with a message that now points at
  // the fix (quote it) instead of the old generic "malformed" message.
  test('an unquoted, dotted import is still a parse error', () => {
    const err = new SimpleErrorChannel()
    const prog = tokenizeAndParse(err, '(import example-defns.scm)')
    expect(prog).toBeUndefined()
    expect(err.errors.length).toBe(1)
    expect(err.errors[0].message).toMatch(/malformed import statement/i)
  })
})
