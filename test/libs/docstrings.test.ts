import { describe, expect, test } from 'vitest'
import { librarySources } from '../../src/lib/generated/sources'
import { tokenizeAndParse } from '../../src/scheme'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import { moduleDocOf } from '../../src/scheme/docstring/module-doc'
import { moduleOrder } from '../../src/app/docs/modules'
import { moduleDocRegistry } from '../../src/lib'

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

/** The program a library source parses to, or undefined if it does not. */
function programOf(module: string, src: string) {
  return tokenizeAndParse(src, undefined, {
    allowInternalNames: module === 'runtime',
  }).program
}

// Module comments (#411) are plumbing for now: the syntax, the registry, the
// docs page and hover all work, but no library has been given one -- that
// content is the maintainer's to write. These pin the shipped state, so the
// first header to land is noticed, and lands in the right place.
describe('module comments in the standard library', () => {
  test('no library has one yet', () => {
    const withComments = librarySources
      .filter(([module, src]) => {
        const program = programOf(module, src)
        return program !== undefined && moduleDocOf(src, program) !== undefined
      })
      .map(([module]) => module)
    // Not a prohibition: when headers are written, update this list. It is here
    // so that adding one is a decision rather than an accident, and so the
    // first is checked against the case below.
    expect(withComments).toEqual([])
    // And the registry agrees with what the parser finds. Asserted together
    // rather than against a literal, so this keeps testing the wiring in
    // src/lib/index.ts once headers exist -- while both are still empty, it is
    // the only thing tying the two together at all.
    expect([...moduleDocRegistry.keys()].sort()).toEqual([...withComments].sort())
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
