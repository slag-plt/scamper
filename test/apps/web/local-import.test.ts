import { describe, expect, test } from 'vitest'
import { uniqueName } from '../../../src/app/web/local-import'

describe('naming a file carried in from the browser', () => {
  test('keeps the name when the account does not have it', () => {
    expect(uniqueName('hello.scm', new Set())).toBe('hello.scm')
  })

  test('says where a clashing copy came from', () => {
    // Two different files with one name, and neither may be discarded. The
    // copy has to say which is which.
    expect(uniqueName('hello.scm', new Set(['hello.scm']))).toBe(
      'hello (from this browser).scm',
    )
  })

  test('counts up when that name is taken too', () => {
    const taken = new Set(['hello.scm', 'hello (from this browser).scm'])
    expect(uniqueName('hello.scm', taken)).toBe(
      'hello (from this browser 2).scm',
    )
  })

  test('keeps the extension where the suffix goes before it', () => {
    // `hello.scm (from this browser)` would stop being a Scheme file as far as
    // anything that looks at the extension is concerned.
    expect(uniqueName('a.b.scm', new Set(['a.b.scm']))).toBe(
      'a.b (from this browser).scm',
    )
  })

  test('handles a name with no extension', () => {
    expect(uniqueName('notes', new Set(['notes']))).toBe(
      'notes (from this browser)',
    )
  })

  test('leaves a dotfile alone rather than splitting on its leading dot', () => {
    expect(uniqueName('.hidden', new Set(['.hidden']))).toBe(
      '.hidden (from this browser)',
    )
  })
})
