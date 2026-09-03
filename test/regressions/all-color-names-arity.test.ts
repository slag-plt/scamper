import { expect, test } from 'vitest'
import { readFileSync } from 'fs'
import { resolve } from 'path'
import { runProgram } from '../harness.js'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import { functionDocSignature } from '../../src/scheme/docstring/render'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// https://github.com/slag-plt/scamper/issues/455
//
// `all-color-names` takes no arguments -- `color_allColorNames` is nullary --
// but its docstring declared a placeholder parameter, `(all-color-names x1)`.
// A docstring is what the contract layer builds a binding's wrapper from, so
// that stray `x1` gave `all-color-names` an arity of one: the correct call
// `(all-color-names)` was an arity error, and the documented call took an
// argument the native then ignored.
//
// N.B., a zero-parameter docstring is deliberately left unwrapped (see
// `contractStmt`), so an extra argument is silently dropped rather than
// rejected -- that is the same for every nullary binding, `rex-empty`
// included, and is not what this test is about.

test('all-color-names takes no arguments (#455)', async () => {
  expect(await runProgram(`
  (import image)
  (list? (all-color-names))
  (string? (car (all-color-names)))
  (color-name? (car (all-color-names)))
  `)).toEqual([
    '#t',
    '#t',
    '#t',
  ])
})

test('all-color-names documents itself as nullary (#455)', () => {
  const src = readFileSync(
    resolve(__dirname, '../../src/lib/image.scm'),
    'utf-8',
  )
  const diagnostics: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(diagnostics, src)
  expect(diagnostics.map((d) => d.message)).toEqual([])
  const stmt = prog.find(
    (s) =>
      (s.tag === 'define' || s.tag === 'defexport') &&
      s.name.name === 'all-color-names',
  )
  expect(stmt).toBeDefined()
  if (stmt?.tag !== 'define' && stmt?.tag !== 'defexport') return
  const { doc } = parseFunctionDocFromComments(stmt.docComments ?? [])
  expect(doc).toBeDefined()
  if (doc === undefined) return
  expect(doc.params).toEqual([])
  expect(doc.optParams).toEqual([])
  expect(doc.restParam).toBeUndefined()
  expect(functionDocSignature(doc)).toBe('(all-color-names) -> list?')
})
