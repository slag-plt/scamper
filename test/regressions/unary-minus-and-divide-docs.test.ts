import { describe, expect, test } from 'vitest'
import { readFileSync } from 'fs'
import { resolve } from 'path'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'
import { runProgram } from '../harness.js'
import { required } from '../dom'

// https://github.com/slag-plt/scamper/issues/676
//
// `-` and `/` each do something at one argument that folding cannot explain:
// `(- 5)` is the additive inverse and `(/ 4)` is the reciprocal, not a
// difference or a quotient of anything. The behaviour is R7RS and deliberate
// -- #517 narrowed both signatures to `(- v1 & v2)` precisely to keep it --
// and `prelude_div` even carries the comment "unary (/ x) means 1/x", but
// the descriptions a student reads said only "Returns the difference of `v1`,
// `v2`, ... ." So the one case that cannot be guessed was the one case
// undocumented.
//
// The two halves here are deliberately paired. Asserting only the prose would
// let it drift into a lie if the natives ever changed; asserting only the
// behaviour is what #517 already does. Together they say: this is what the
// docs promise, and this is the promise holding.
//
// A sweep for siblings found none -- `+`, `*`, `max`, `min`, `append`,
// `string` and `string-append` all fold uniformly at one argument -- so these
// two are the whole class.

/** The parsed description of `name`'s docstring in the prelude. */
function descriptionOf(name: string): string {
  const src = readFileSync(
    resolve(__dirname, '../../src/lib/prelude.scm'),
    'utf-8',
  )
  const diagnostics: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(diagnostics, src)
  expect(diagnostics.map((d) => d.message)).toEqual([])
  const def = prog.find(
    (s) =>
      (s.tag === 'define' || s.tag === 'defexport') && s.name.name === name,
  )
  const comments = required(
    def?.tag === 'define' || def?.tag === 'defexport'
      ? def.docComments
      : undefined,
    `a docstring on ${name}`,
  )
  const { doc, diagnostics: docDiagnostics } =
    parseFunctionDocFromComments(comments)
  expect(docDiagnostics.map((d) => d.message)).toEqual([])
  return required(doc, `a parsed docstring on ${name}`).description
}

describe('the one-argument - and / are documented (#676)', () => {
  test("- 's description names the additive inverse", () => {
    expect(descriptionOf('-')).toContain('additive inverse')
  })

  test("/ 's description names the reciprocal", () => {
    expect(descriptionOf('/')).toContain('reciprocal')
  })

  test('and that is still what one argument does', async () => {
    expect(
      await runProgram(`
(- 5)
(/ 4)
`),
    ).toEqual(['-5', '0.25'])
  })
})
