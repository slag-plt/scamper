import { describe, expect, test } from 'vitest'
import { readFileSync } from 'fs'
import { resolve } from 'path'
import * as A from '../../src/scheme/ast'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import {
  FunctionDoc,
  parseFunctionDocFromComments,
} from '../../src/scheme/docstring/docstring'
import { functionDocSignature } from '../../src/scheme/docstring/render'
import { parseSignature } from '../../src/scheme/docstring/signature'
import { Range } from '../../src/lpm'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// #412: `null` was missing from the documentation entirely. Its docstring said
// `(null) -> list?`, and the signature parser validated that head as an
// identifier -- which `null` is not, being a literal in Scamper source. The
// line could not be written, so `extractDocs` dropped the binding.
//
// Behind that was a notation gap: neither `null` nor `pi` is callable, and
// `(null) -> list?` describes a nullary function that does not exist. A
// constant is now written `null: list?`, which both parses and reads correctly.

function definesByName(file: string): Map<string, A.Define | A.DefineExport> {
  const src = readFileSync(resolve(__dirname, `../../src/lib/${file}`), 'utf-8')
  const diagnostics: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(diagnostics, src)
  expect(diagnostics.map((d) => d.message)).toEqual([])
  const byName = new Map<string, A.Define | A.DefineExport>()
  for (const s of prog) {
    if (s.tag === 'define' || s.tag === 'defexport') {
      byName.set(s.name.name, s)
    }
  }
  return byName
}

function docFor(
  defs: Map<string, A.Define | A.DefineExport>,
  name: string,
): FunctionDoc {
  const comments = defs.get(name)?.docComments
  if (comments === undefined) {
    throw new Error(`expected ${name} to have a docstring`)
  }
  const { doc, diagnostics } = parseFunctionDocFromComments(comments)
  expect(
    diagnostics.map((d) => d.message),
    `${name}'s docstring should parse`,
  ).toEqual([])
  if (doc === undefined) {
    throw new Error(`expected a parsed docstring for ${name}`)
  }
  return doc
}

describe('a constant can say that it is one (#412)', () => {
  const prelude = definesByName('prelude.scm')

  test.each([
    ['null', 'list?'],
    ['pi', 'number?'],
    ['else', 'boolean?'],
    ['void', 'void?'],
  ])('%s documents as a constant, and renders as one', (name, pred) => {
    const doc = docFor(prelude, name)
    expect(doc.signature.isConstant).toBe(true)
    expect(doc.signature.function.head.name).toBe(name)
    expect(doc.signature.function.args).toEqual([])
    expect(functionDocSignature(doc)).toBe(`${name}: ${pred}`)
  })

  describe('the notation itself', () => {
    const parse = (line: string) => parseSignature({ line, range: Range.none })

    test('a constant is a name, a colon, and a predicate', () => {
      const sig = parse('pi: number?')
      expect(sig.isConstant).toBe(true)
      expect(sig.function.head.name).toBe('pi')
    })

    test('spacing around the colon is not load-bearing', () => {
      expect(parse('pi : number?').function.head.name).toBe('pi')
    })

    test('a complex predicate works too', () => {
      expect(parse('empty: (list-of number?)').isConstant).toBe(true)
    })

    test('a constant takes no parameters', () => {
      expect(() => parse('pi x: number?')).toThrow(/takes no parameters/)
    })

    test('a line that is neither form says what it expected', () => {
      expect(() => parse('pi number?')).toThrow(/name: predicate/)
    })

    test('a function still needs its arrow, and its head an identifier', () => {
      expect(() => parse('(pi) number?')).toThrow(/Missing separator/)
      expect(() => parse('(null) -> list?')).toThrow(/Expected an identifier/)
    })
  })

  test('a nullary function still shows that it must be called', () => {
    // `rex-empty` takes no arguments but is not a constant, and used to render
    // exactly as `pi` did -- with nothing to say it was callable.
    const doc = docFor(definesByName('rex.scm'), 'rex-empty')
    expect(doc.signature.isConstant).toBe(false)
    expect(functionDocSignature(doc)).toBe('(rex-empty) -> rex?')
  })
})
