import { describe, expect, test } from 'vitest'
import { librarySources } from '../../src/lib/generated/sources'
import { tokenizeAndParse } from '../../src/scheme'
import { moduleDocOf } from '../../src/scheme/docstring/module-doc'
import { moduleOrder } from '../../src/app/docs/modules'
import { moduleDocRegistry } from '../../src/lib'
import { libraryBindings } from './library-bindings'

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
  return new Map(
    libraryBindings()
      .filter((b) => b.diagnostics.length > 0)
      .map((b) => [`${b.module}:${b.name}`, b.diagnostics[0].message]),
  )
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

/** The program a library source parses to, or undefined if it does not. */
function programOf(module: string, src: string) {
  return tokenizeAndParse(src, undefined, {
    allowInternalNames: module === 'runtime',
  }).program
}

// Every module the docs cover says what it is for (#411). A module whose header
// is written wrongly -- with no blank line under it -- does not merely go
// unsaid: it is swallowed by the first definition below, which then loses both
// its documentation and the contract derived from it.
describe('module comments in the standard library', () => {
  test('every documented module has one, and runtime does not', () => {
    const withComments = librarySources
      .filter(([module, src]) => {
        const program = programOf(module, src)
        return program !== undefined && moduleDocOf(src, program) !== undefined
      })
      .map(([module]) => module)
    // runtime is LPM interop rather than a library anyone imports, so it is
    // deliberately absent -- which is also why the docs exclude it.
    expect(withComments.sort()).toEqual([...moduleOrder].sort())
    // And the registry agrees with what the parser finds, which is what ties
    // the wiring in src/lib/index.ts to the extraction it depends on.
    expect([...moduleDocRegistry.keys()].sort()).toEqual([...withComments].sort())
  })

  test('each one says something', () => {
    // A header that parsed to an empty blurb would pass the test above and
    // still show a blank line on the docs page.
    for (const module of moduleOrder) {
      expect(
        moduleDocRegistry.get(module)?.description,
        `${module} has no module comment`,
      ).toBeTruthy()
    }
  })

  test("every documented library's first definition keeps its own docstring", () => {
    // The mistake a header invites: written with no blank line under it, it is
    // swallowed by the first define, which then loses both its documentation
    // and the contract derived from it.
    //
    // Over the modules the docs cover, which is what `moduleOrder` is: runtime
    // is LPM interop and carries no docstrings at all, so it has nothing here
    // to lose.
    const undocumented = librarySources
      .filter(([module]) => moduleOrder.includes(module))
      .filter(([module, src]) => {
        const first = programOf(module, src)?.find(
          (stmt) => stmt.tag === 'define' || stmt.tag === 'defexport',
        )
        return (
          (first?.tag === 'define' || first?.tag === 'defexport') &&
          first.docComments === undefined
        )
      })
      .map(([module]) => module)
    expect(undocumented).toEqual([])
  })
})
