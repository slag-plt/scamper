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
import { contractStmt } from '../../src/scheme/contract'
import { runProgram } from '../harness.js'
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

// https://github.com/slag-plt/scamper/issues/469
//
// `contractStmt` builds a wrapper out of a docstring's *parameters*, so a
// signature naming none has nothing to check and the define is returned
// untouched. Both zero-parameter forms land there: a constant (`pi`,
// `isConstant` true) and a nullary function (`rex-empty`, `isConstant` false).
// Since #412 the parsed signature does tell the two apart, so leaving a
// nullary function unwrapped is now a choice rather than a limitation --
// these tests pin it, so changing it has to be deliberate.
describe('a zero-parameter docstring is left unwrapped (#469)', () => {
  const stmtFor = (
    defs: Map<string, A.Define | A.DefineExport>,
    name: string,
  ): A.Define | A.DefineExport => {
    const stmt = defs.get(name)
    if (stmt === undefined) {
      throw new Error(`expected a definition of ${name}`)
    }
    return stmt
  }

  test('a constant is not wrapped, and is still used as a value', async () => {
    const prelude = definesByName('prelude.scm')
    expect(docFor(prelude, 'pi').signature.isConstant).toBe(true)
    const stmt = stmtFor(prelude, 'pi')
    expect(contractStmt(stmt)).toBe(stmt)
    expect(await runProgram('(> pi 3)')).toEqual(['#t'])
  })

  test('a nullary function is not wrapped either, and is still called', async () => {
    const rex = definesByName('rex.scm')
    expect(docFor(rex, 'rex-empty').signature.isConstant).toBe(false)
    const stmt = stmtFor(rex, 'rex-empty')
    expect(contractStmt(stmt)).toBe(stmt)
    expect(await runProgram('(import rex)\n(rex? (rex-empty))')).toEqual(['#t'])
  })

  test('a rest-only signature is wrapped, having something to check', () => {
    // `(rex-concat & xs) -> rex?`: no fixed parameters, but `xs : rex?` is a
    // predicate to apply, so this one does get its wrapper.
    const rex = definesByName('rex.scm')
    const doc = docFor(rex, 'rex-concat')
    expect(doc.params).toEqual([])
    expect(doc.restParam?.name).toBe('xs')
    const stmt = stmtFor(rex, 'rex-concat')
    expect(contractStmt(stmt)).not.toBe(stmt)
  })
})
