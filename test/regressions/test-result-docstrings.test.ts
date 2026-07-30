import { describe, expect, test } from 'vitest'
import { readFileSync } from 'fs'
import { resolve } from 'path'
import * as A from '../../src/scheme/ast'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import { isCategoryTag } from '../../src/scheme/docstring/tags/category-tag'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// Regression test for #77: "test-error doesn't work as documented". The `test`
// library's result constructors (test-result-ok / -error-expected /
// -error-exn / -error-gen) had no docstrings, so they never appeared on the
// docs site. Users therefore reached for the old, documented `test-error` name
// -- renamed to `test-result-error-gen` in 3.4.0 -- which no longer exists.
// Documenting the constructors resolves the confusion.

const DOCUMENTED = [
  'test-result-ok',
  'test-result-error-expected',
  'test-result-error-exn',
  'test-result-error-gen',
]

function definesByName(): Map<string, A.Define> {
  const src = readFileSync(resolve(__dirname, '../../src/lib/test.scm'), 'utf-8')
  const diagnostics: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(diagnostics, src)
  expect(diagnostics.map((d) => d.message)).toEqual([])
  const byName = new Map<string, A.Define>()
  for (const s of prog) {
    if (s.tag === 'define') {
      byName.set(s.name.name, s)
    }
  }
  return byName
}

describe('#77: test result constructors are documented', () => {
  const defs = definesByName()

  test.each(DOCUMENTED)(
    '%s has a valid, testing-categorized docstring',
    (name) => {
      const def = defs.get(name)
      expect(def, `expected a define for ${name}`).toBeDefined()
      expect(def?.docComments, `${name} should have a docstring`).toBeTruthy()

      const { doc, diagnostics } = parseFunctionDocFromComments(def!.docComments!)
      expect(diagnostics.map((d) => d.message)).toEqual([])
      expect(doc).toBeDefined()
      expect(doc?.signature.function.head.name).toBe(name)

      const categories = (doc?.tags ?? [])
        .filter(isCategoryTag)
        .flatMap((t) => t.contents)
      expect(categories).toContain('testing')
    },
  )
})
