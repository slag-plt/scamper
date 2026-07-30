import { afterEach, describe, expect, test, vi } from 'vitest'
import * as fs from '../../src/fs'
import { scopeCheckProgram } from '../../src/scheme/scope'
import { expandProgram } from '../../src/scheme/expansion'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'

// Regression test for #283 and #284.
//
// #283: scope checking flagged forward references between top-level definitions,
// so mutual recursion at the top level was rejected even though it is legal.
// #284: a name collision between a `define` and an `import` was an error in one
// ordering but silently accepted in the other.
//
// Both are resolved by treating the top level as one mutually-recursive scope
// (Racket module semantics: every module-level definition and import shares one
// scope covering the whole body, so position is not significant, and "an
// identifier can be either imported or defined ... but not both").

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
  const parseErrs: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(parseErrs, src)
  expect(parseErrs, 'test source should parse cleanly').toEqual([])
  await scopeCheckProgram(errors, expandProgram(prog))
  return errors.map((e) => e.message)
}

afterEach(() => {
  vi.restoreAllMocks()
})

describe('#283: top-level definitions are mutually recursive', () => {
  test('mutual recursion between two top-level defines is accepted', async () => {
    // N.B., fresh names -- redefining prelude bindings (e.g. even?/odd?) is a
    // separate, still-flagged collision.
    expect(
      await scopeErrors(
        '(define my-even? (lambda (n) (if (zero? n) #t (my-odd? (- n 1)))))\n' +
          '(define my-odd? (lambda (n) (if (zero? n) #f (my-even? (- n 1)))))',
      ),
    ).toEqual([])
  })

  test('a forward reference to a later define resolves', async () => {
    expect(await scopeErrors('x\n(define x 1)')).toEqual([])
  })

  test('a still-undefined name is reported', async () => {
    expect(await scopeErrors('(define f (lambda () (ghost)))')).toEqual([
      "Undefined variable 'ghost'",
    ])
  })
})

describe('#284: define/import collisions are order-independent', () => {
  test('import then same-named define collides', async () => {
    mockFS({ 'utils.scm': '(define helper 1)' })
    expect(
      await scopeErrors('(import "utils.scm")\n(define helper 2)'),
    ).toEqual(["Global variable 'helper' is already defined"])
  })

  test('define then same-named import collides (symmetric)', async () => {
    mockFS({ 'utils.scm': '(define helper 1)' })
    expect(
      await scopeErrors('(define helper 1)\n(import "utils.scm")'),
    ).toEqual(["Global variable 'helper' is already defined"])
  })

  test('two imports of the same name from different modules collide', async () => {
    mockFS({ 'x.scm': '(define dup 1)', 'y.scm': '(define dup 2)' })
    expect(await scopeErrors('(import "x.scm")\n(import "y.scm")')).toEqual([
      "Global variable 'dup' is already defined",
    ])
  })

  test('re-importing the same module is not a collision', async () => {
    mockFS({ 'utils.scm': '(define helper 1)' })
    expect(
      await scopeErrors('(import "utils.scm")\n(import "utils.scm")'),
    ).toEqual([])
  })

  test('a library import overlapping a standard-library name is not flagged', async () => {
    // Only *user-introduced* collisions are flagged; importing a module whose
    // export happens to match a prelude/runtime name must stay clean.
    expect(await scopeErrors('(import image)')).toEqual([])
  })
})
