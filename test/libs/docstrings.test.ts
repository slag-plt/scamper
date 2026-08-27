import { expect, test } from 'vitest'
import { librarySources } from '../../src/lib/generated/sources'
import { tokenizeAndParse } from '../../src/scheme'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'

// A malformed docstring in the standard library is invisible: extractDocs
// (src/lib/index.ts) keeps the parsed doc and drops the diagnostic, so the
// binding simply goes missing from the documentation -- and, because contract
// insertion reads the same docstring, silently loses its contract too. This is
// the guard: the diagnostic the library load path throws away, asserted on here
// instead.
//
// It found two when it was written. `canvas`'s `fill-mode?` had an N.B. note
// written with `;;;` after its @category line, which made it a docstring line
// the parser could not place; `null` could not say what it was, having no
// notation for a constant. Both are fixed, and the list below is empty.

/**
 * Bindings whose docstring is known not to parse, and why. A new entry needs a
 * reason and an issue -- it means a documented binding is missing from the
 * docs page.
 */
const KNOWN_BAD = new Map<string, string>([])

/** Every `<module>:<name>` in the standard library whose docstring won't parse. */
function malformedDocstrings(): Map<string, string> {
  const bad = new Map<string, string>()
  for (const [module, src] of librarySources) {
    const { program } = tokenizeAndParse(src, undefined, {
      allowInternalNames: module === 'runtime',
    })
    for (const stmt of program ?? []) {
      if (
        (stmt.tag !== 'define' && stmt.tag !== 'defexport') ||
        stmt.docComments === undefined
      ) {
        continue
      }
      const { diagnostics } = parseFunctionDocFromComments(stmt.docComments)
      if (diagnostics.length > 0) {
        bad.set(`${module}:${stmt.name.name}`, diagnostics[0].message)
      }
    }
  }
  return bad
}

test('no library docstring is malformed, beyond the known exceptions', () => {
  const bad = malformedDocstrings()
  const unexpected = [...bad].filter(([key]) => !KNOWN_BAD.has(key))
  expect(
    unexpected.map(([key, message]) => `${key}: ${message}`),
    'a malformed docstring costs its binding both its documentation and its contract',
  ).toEqual([])
})

test('the known exceptions are still broken, so they can be retired when fixed', () => {
  const bad = malformedDocstrings()
  for (const [key, message] of KNOWN_BAD) {
    expect(bad.get(key), `${key} now parses -- drop it from KNOWN_BAD`).toContain(
      message,
    )
  }
})
