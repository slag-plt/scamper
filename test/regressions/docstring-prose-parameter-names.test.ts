import { describe, expect, test } from 'vitest'
import { readFileSync, readdirSync } from 'fs'
import { resolve } from 'path'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import { reservedWords } from '../../src/scheme/reserved-words'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// https://github.com/slag-plt/scamper/issues/594
//
// `rex-concat` and `rex-any-of` described a parameter `rs` that their
// signatures call `xs`. `checkNamesMatch` (see docstring.ts) already pins the
// `name : predicate` contract lines against the signature, but nothing checked
// the *prose*, which is what the docs site shows a student underneath that
// signature -- so a description could name a parameter that is not there.
//
// This sweeps every binding in the standard library rather than the two sites
// the issue names, so the whole class is guarded.

const libDir = resolve(__dirname, '../../src/lib')

/**
 * Prose refers to code in backticks. Only letter-initial names are considered:
 * that skips literals (`#t`, `-1`, `.png`) and symbolic bindings (`|>`, `<=`),
 * none of which can be confused with a parameter.
 */
const nameRegex = /^[A-Za-z][A-Za-z0-9!?*<>=+_-]*$/

/** The backticked spans of `prose`, e.g. "matches `r`" -> ["r"]. */
function backtickedSpans(prose: string): string[] {
  return [...prose.matchAll(/`([^`]*)`/g)].map((m) => m[1])
}

function words(text: string): string[] {
  return text.split(/[^A-Za-z0-9!?*<>=+_-]+/).filter((w) => w !== '')
}

/**
 * The names a docstring brings into scope itself, so that later prose may
 * refer to them: anything inside a parenthesized form -- `on-timer`'s
 * "a message of the form `(event-timer time elapsed)`" -- or inside a
 * multi-word backticked expression, like `modulo`'s "`k = n - d * q`".
 */
function namesIntroducedByProse(prose: string): Set<string> {
  const introduced = [...prose.matchAll(/\(([^)]*)\)/g)].flatMap((m) =>
    words(m[1]),
  )
  const expressions = backtickedSpans(prose)
    .filter((span) => /\s/.test(span))
    .flatMap(words)
  return new Set([...introduced, ...expressions])
}

/** `"v2"` -> `"v"`, `"vk"` -> `"v"`, `"xs"` -> undefined. */
function indexedBase(name: string): string | undefined {
  const match = /^(.+?)([0-9]+|k)$/.exec(name)
  return match ? match[1] : undefined
}

/**
 * A variadic's description numbers its arguments off the parameter it is
 * given: `(+ v1 & v2)` is described in terms of `v1`, `v2`, ... `vk`. Those
 * count as references to the parameter they are indexing.
 */
function isIndexedSiblingOf(name: string, params: string[]): boolean {
  const base = indexedBase(name)
  return (
    base !== undefined &&
    params.some((p) => p === base || indexedBase(p) === base)
  )
}

interface DocumentedBinding {
  file: string
  name: string
  params: string[]
  prose: string
}

/** Every documented binding in `src/lib/*.scm`, with the names it may refer to. */
function readLibrary(): {
  bindings: DocumentedBinding[]
  libraryNames: Set<string>
  moduleNames: Set<string>
} {
  const files = readdirSync(libDir).filter((f) => f.endsWith('.scm'))
  const bindings: DocumentedBinding[] = []
  const libraryNames = new Set<string>()
  for (const file of files) {
    const diagnostics: ScamperDiagnostic[] = []
    const program = parseProgramFromSource(
      diagnostics,
      readFileSync(resolve(libDir, file), 'utf-8'),
      // runtime.scm is the interop layer, and is the one module allowed to
      // bind the reserved `##...##` names -- see lib/index.ts.
      { allowInternalNames: file === 'runtime.scm' },
    )
    expect(diagnostics.map((d) => d.message), `${file} should parse`).toEqual([])
    for (const stmt of program) {
      if (stmt.tag !== 'define' && stmt.tag !== 'defexport') {
        continue
      }
      libraryNames.add(stmt.name.name)
      const { doc } = parseFunctionDocFromComments(stmt.docComments ?? [])
      if (doc === undefined) {
        continue
      }
      const params = [
        ...doc.params,
        ...doc.optParams,
        ...(doc.restParam ? [doc.restParam] : []),
      ]
      bindings.push({
        file,
        name: stmt.name.name,
        params: params.map((p) => p.name),
        prose: [doc.description, ...params.map((p) => p.description ?? '')].join(
          ' ',
        ),
      })
    }
  }
  return {
    bindings,
    libraryNames,
    moduleNames: new Set(files.map((f) => f.replace(/\.scm$/, ''))),
  }
}

/**
 * Pre-existing prose in the same shape as #594, left alone so that fixing it
 * does not ride along with an unrelated change. Each entry is
 * "<file> <binding> <name the prose uses>"; remove an entry when its docstring
 * is corrected. Do not add to this list -- a new mismatch is a bug to fix.
 */
const knownMismatches = new Set([
  'image.scm find-colors color', // the parameter is `color-name`
  'image.scm isosceles-triangle base', // the parameter is `width`
  'image.scm solid-isosceles-triangle base', // the parameter is `width`
  'image.scm outlined-isosceles-triangle base', // the parameter is `width`
  'music.scm instrument comp', // there is no `comp` parameter
  'music.scm make-note-handlers note-handler', // the binding is `note-handlers`
])

describe('a docstring only names parameters its signature declares (#594)', () => {
  const { bindings, libraryNames, moduleNames } = readLibrary()

  /** The backticked names in `binding`'s prose that resolve to nothing. */
  function danglingNames(binding: DocumentedBinding): string[] {
    const introduced = namesIntroducedByProse(binding.prose)
    return backtickedSpans(binding.prose).filter(
      (name) =>
        nameRegex.test(name) &&
        // the one literal that reads as a name; `#t`/`-1` are already skipped
        name !== 'NaN' &&
        !binding.params.includes(name) &&
        !isIndexedSiblingOf(name, binding.params) &&
        !libraryNames.has(name) &&
        !moduleNames.has(name) &&
        !reservedWords.includes(name) &&
        !introduced.has(name),
    )
  }

  test('the library is actually being read', () => {
    expect(bindings.length).toBeGreaterThan(400)
    expect(libraryNames.has('rex-concat')).toBe(true)
  })

  test('every backticked name in a docstring resolves to something', () => {
    const dangling = bindings.flatMap((b) =>
      [...new Set(danglingNames(b))].map(
        (name) => `${b.file} ${b.name} ${name}`,
      ),
    )
    expect(dangling.filter((d) => !knownMismatches.has(d))).toEqual([])
  })

  // The two sites the issue names, pinned directly so a regression there is
  // reported as itself rather than as one line of the sweep above.
  test.each([['rex-concat'], ['rex-any-of']])(
    "%s's description names its rest parameter",
    (name) => {
      const binding = bindings.find(
        (b) => b.file === 'rex.scm' && b.name === name,
      )
      if (binding === undefined) {
        throw new Error(`expected rex.scm to define ${name}`)
      }
      expect(binding.params).toEqual(['xs'])
      expect(danglingNames(binding)).toEqual([])
    },
  )

  test('the allowlist has no stale entries', () => {
    const dangling = new Set(
      bindings.flatMap((b) =>
        danglingNames(b).map((name) => `${b.file} ${b.name} ${name}`),
      ),
    )
    expect([...knownMismatches].filter((m) => !dangling.has(m))).toEqual([])
  })
})
