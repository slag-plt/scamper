import { describe, expect, test } from 'vitest'
import { tokenizeAndParse } from '../../src/scheme'

// A bare-identifier import, e.g. `(import lists)`, denotes a built-in
// library. A quoted-string import, e.g. `(import "my-file.scm")`, denotes a
// user file. These two forms are distinguished at parse time -- not, as
// before, by probing for the name's existence at runtime -- because a built-in
// library name is always a simple identifier, whereas a file name routinely
// contains a "." (its extension). A dotted, unquoted name now tokenizes as a
// *qualified* identifier (see syntax.grammar), which is illegal as a module
// name, so `(import example-defns.scm)` is still a parse error -- but with a
// message that points at the fix (quote it).

describe('import: built-in library vs. file', () => {
  test('a bare identifier import is parsed as a built-in library', () => {
    const { program: prog, diagnostics } = tokenizeAndParse('(import prelude)')
    expect(diagnostics).toEqual([])
    expect(prog?.length).toBe(1)
    const stmt = prog?.[0]
    expect(stmt?.tag).toBe('import')
    if (stmt?.tag !== 'import') return
    expect(stmt.module).toBe('prelude')
    expect(stmt.kind).toBe('builtin')
  })

  test("a quoted file import (including one with a '.' in its name) is parsed as a file", () => {
    const { program: prog, diagnostics } = tokenizeAndParse(
      '(import "example-defns.scm")',
    )
    expect(diagnostics).toEqual([])
    expect(prog?.length).toBe(1)
    const stmt = prog?.[0]
    expect(stmt?.tag).toBe('import')
    if (stmt?.tag !== 'import') return
    expect(stmt.module).toBe('example-defns.scm')
    expect(stmt.kind).toBe('file')
  })

  // A file import must be quoted -- an unquoted dotted name is a qualified
  // identifier, illegal as a module name -- so this remains a parse error,
  // with a message that points at the fix (quote it).
  test('an unquoted, dotted import is still a parse error', () => {
    const { program: prog, diagnostics } = tokenizeAndParse(
      '(import example-defns.scm)',
    )
    expect(prog).toBeUndefined()
    expect(diagnostics.length).toBe(1)
    expect(diagnostics[0].message).toMatch(/malformed import statement/i)
    expect(diagnostics[0].message).toMatch(/must be quoted/i)
  })
})
