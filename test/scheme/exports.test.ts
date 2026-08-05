import { describe, test, expect, vi, afterEach } from 'vitest'
import * as fs from '../../src/fs'
import * as A from '../../src/scheme/ast'
import * as S from '../../src/scheme'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { expandProgram } from '../../src/scheme/expansion'
import { sugarProgram } from '../../src/scheme/sugar'
import { scopeCheckProgram } from '../../src/scheme/scope'
import { Fiber } from '../../src/lpm/fiber'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// Explicit module exports: (export ...) and its sugar (define-export x e). A
// module exports only the names its export statements name; the union across
// all of them is what an import sees. See scope.ts / Fiber.getModule.

function parse(src: string): A.Prog {
  const errs: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(errs, src)
  expect(errs.map((e) => e.message), 'source should parse cleanly').toEqual([])
  return prog
}

function parseWithErrors(src: string): ScamperDiagnostic[] {
  const errs: ScamperDiagnostic[] = []
  parseProgramFromSource(errs, src)
  return errs
}

async function scopeErrors(src: string): Promise<string[]> {
  const errors: ScamperDiagnostic[] = []
  await scopeCheckProgram(errors, expandProgram(parse(src)))
  return errors.map((e) => e.message)
}

function mockFS(files: Record<string, string>): void {
  vi.spyOn(fs, 'getFS').mockReturnValue({
    fileExists: (f: string) => Promise.resolve(f in files),
    loadFile: (f: string) =>
      f in files
        ? Promise.resolve(files[f])
        : Promise.reject(new Error(`no such file: ${f}`)),
  } as unknown as ReturnType<typeof fs.getFS>)
}

/** Runs `src` to completion and returns the names its module exports. */
async function moduleExports(src: string): Promise<string[]> {
  const { prog, diagnostics } = await S.compile(src)
  expect(diagnostics.map((d) => d.message)).toEqual([])
  if (prog === undefined) throw new Error('compile produced no program')
  const fiber = new Fiber(prog, S.mkInitialEnv())
  while (!fiber.isDone()) fiber.step()
  return [...fiber.getModule().bindings.keys()].sort()
}

afterEach(() => {
  vi.restoreAllMocks()
})

describe('parsing', () => {
  test('(export ...) parses to an Export with its names', () => {
    const s = parse('(export a b c)')[0] as A.Export
    expect(s.tag).toBe('export')
    expect(s.names.map((n) => n.name)).toEqual(['a', 'b', 'c'])
  })

  test('an empty (export) is allowed', () => {
    const s = parse('(export)')[0] as A.Export
    expect(s.tag).toBe('export')
    expect(s.names).toEqual([])
  })

  test('(define-export x e) parses to a DefineExport', () => {
    const s = parse('(define-export x 5)')[0] as A.DefineExport
    expect(s.tag).toBe('defexport')
    expect(s.name.name).toBe('x')
    expect(s.value.tag).toBe('lit')
  })

  test('a qualified name in an export is rejected', () => {
    expect(parseWithErrors('(export a.b)').length).toBeGreaterThan(0)
  })

  test('a qualified name as a define-export binder is rejected', () => {
    expect(parseWithErrors('(define-export a.b 5)').length).toBeGreaterThan(0)
  })
})

describe('expansion + sugaring round trip', () => {
  const roundTrip = (src: string): string =>
    A.progToString(sugarProgram(expandProgram(parse(src))))

  test('define-export expands to a define + export pair', () => {
    const expanded = expandProgram(parse('(define-export f 5)'))
    expect(expanded.map((s) => s.tag)).toEqual(['define', 'export'])
    expect((expanded[1] as A.Export).names.map((n) => n.name)).toEqual(['f'])
  })

  test('sugaring recovers a define-export from its expansion', () => {
    expect(roundTrip('(define-export f 5)')).toBe('(define-export f 5)')
  })

  test('the recovered define-export re-sugars a derived form in its value', () => {
    expect(roundTrip('(define-export f (and a b))')).toBe(
      '(define-export f (and a b))',
    )
  })

  test('a hand-written define + export pair is NOT collapsed (no over-sugaring)', () => {
    // No define-export provenance tags these, so they stay two statements.
    expect(roundTrip('(define f 5)\n(export f)')).toBe('(define f 5)\n(export f)')
  })

  test('a plain export round-trips unchanged', () => {
    expect(roundTrip('(export a b)')).toBe('(export a b)')
  })
})

describe('scope checking (strict exports)', () => {
  test('a module exports only the names it lists', async () => {
    mockFS({ 'm.scm': '(define foo 1)\n(define bar 2)\n(export foo)' })
    expect(await scopeErrors('(import "m.scm")\nfoo')).toEqual([])
    expect(await scopeErrors('(import "m.scm")\nbar')).toEqual([
      "Undefined variable 'bar'",
    ])
  })

  test('define-export exports its name', async () => {
    mockFS({ 'm.scm': '(define-export g 1)' })
    expect(await scopeErrors('(import "m.scm")\ng')).toEqual([])
  })

  test('a module with no export statement exports nothing', async () => {
    mockFS({ 'm.scm': '(define foo 1)' })
    expect(await scopeErrors('(import "m.scm")\nfoo')).toEqual([
      "Undefined variable 'foo'",
    ])
  })

  test('the union of several export statements is exported', async () => {
    mockFS({ 'm.scm': '(define a 1)\n(define b 2)\n(export a)\n(export b)' })
    expect(await scopeErrors('(import "m.scm")\n(+ a b)')).toEqual([])
  })

  test('exporting an undefined name is flagged', async () => {
    expect(await scopeErrors('(export nope)')).toEqual([
      "Exporting undefined variable 'nope'",
    ])
  })

  test('exporting a name defined in this module is fine', async () => {
    expect(await scopeErrors('(define foo 1)\n(export foo)')).toEqual([])
  })

  test('cannot export a name that was imported, not defined here', async () => {
    expect(await scopeErrors('(import image)\n(export rgb)')).toEqual([
      "Cannot export 'rgb': it is not defined in this module",
    ])
  })
})

describe('runtime module snapshot', () => {
  test('only exported names appear in the module', async () => {
    expect(await moduleExports('(define-export a 1)\n(define b 2)')).toEqual(['a'])
  })

  test('a program with no export statement exports nothing', async () => {
    expect(await moduleExports('(define a 1)\n(define b 2)')).toEqual([])
  })

  test('a plain export exports the named top-level bindings', async () => {
    expect(await moduleExports('(define a 1)\n(define b 2)\n(export a b)')).toEqual(
      ['a', 'b'],
    )
  })

  test('exporting a name the program does not bind exports nothing for it', async () => {
    expect(await moduleExports('(define a 1)\n(export a missing)')).toEqual(['a'])
  })
})
