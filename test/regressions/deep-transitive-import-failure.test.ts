import { afterEach, describe, expect, test, vi } from 'vitest'
import * as fs from '../../src/fs'
import { scopeCheckProgram } from '../../src/scheme/scope'
import { expandProgram } from '../../src/scheme/expansion'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// Regression test for #285: a missing or unparseable module deep in the import
// graph was skipped silently and never reported, even though a *direct* import
// failure is reported. Scope checking only re-derived diagnostics for the
// top-level program's own imports, so a failure below the first level produced
// no error at all -- names a deep module should export went missing with no
// explanation, surfacing later only as confusing "Undefined variable" errors.
//
// The fix has loadTransitiveImports collect below-top-level failures and
// scopeCheckProgram surface them, pointing at the top-level import that pulled
// the broken module in.

function mockFS(files: Record<string, string>): void {
  vi.spyOn(fs, 'getFS').mockReturnValue({
    fileExists: (f: string) => Promise.resolve(f in files),
    loadFile: (f: string) =>
      f in files
        ? Promise.resolve(files[f])
        : Promise.reject(new Error(`no such file: ${f}`)),
  } as unknown as ReturnType<typeof fs.getFS>)
}

async function scopeErrors(src: string): Promise<string[]> {
  const errors: ScamperDiagnostic[] = []
  const errs: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(errs, src)
  expect(errs, 'test source should parse cleanly').toEqual([])
  await scopeCheckProgram(errors, expandProgram(prog))
  return errors.map((e) => e.message)
}

afterEach(() => {
  vi.restoreAllMocks()
})

describe('#285: deep transitive import failures are surfaced', () => {
  test('an unparseable module one level down is reported', async () => {
    mockFS({ 'a.scm': '(import "c.scm")\n(define-export fromA 1)', 'c.scm': '(1 2' })
    expect(await scopeErrors('(import "a.scm")\nfromA')).toEqual([
      "Could not load module 'c.scm' (imported by 'a.scm')",
    ])
  })

  test('a missing module one level down is reported', async () => {
    mockFS({ 'a.scm': '(import "gone.scm")\n(define-export fromA 1)' })
    expect(await scopeErrors('(import "a.scm")\nfromA')).toEqual([
      "Could not load module 'gone.scm' (imported by 'a.scm')",
    ])
  })

  test('the failure names its direct importer but points at the top-level import', async () => {
    // prog -> a.scm -> b.scm -> c.scm(broken): c.scm's direct importer is
    // b.scm, but the reported location is the top-level `(import "a.scm")`.
    mockFS({
      'a.scm': '(import "b.scm")\n(define-export fromA 1)',
      'b.scm': '(import "c.scm")\n(define-export fromB 2)',
      'c.scm': '(1 2',
    })
    expect(await scopeErrors('(import "a.scm")\nfromA')).toEqual([
      "Could not load module 'c.scm' (imported by 'b.scm')",
    ])
  })

  test('a healthy transitive graph still reports nothing', async () => {
    mockFS({
      'a.scm': '(import "b.scm")\n(define-export fromA 1)',
      'b.scm': '(define-export fromB 2)',
    })
    expect(await scopeErrors('(import "a.scm")\nfromA')).toEqual([])
  })
})
