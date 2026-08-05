import { describe, test, expect, vi, afterEach } from 'vitest'
import * as fs from '../../src/fs'
import { scopeCheckProgram } from '../../src/scheme/scope'
import { expandProgram } from '../../src/scheme/expansion'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'
import { runProgram } from '../harness.js'

// End-to-end coverage of qualified imports: `(import m alias)` makes m's exports
// reachable only as `alias.member`, never injected unqualified. Scope checking
// validates the alias/member pair; the runtime resolves it through Env's
// qualified map. The symbol DB + builtin libs are initialized by test/setup.ts.

function parse(src: string) {
  const errs: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(errs, src)
  expect(
    errs.map((e) => e.message),
    'test source should itself parse cleanly',
  ).toEqual([])
  return prog
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

afterEach(() => {
  vi.restoreAllMocks()
})

describe('qualified imports: scope checking', () => {
  test('a qualified reference to an existing export is well-scoped', async () => {
    expect(await scopeErrors('(import image img)\nimg.rgb-red')).toEqual([])
  })

  test("a qualified import does NOT inject the module's names unqualified", async () => {
    expect(await scopeErrors('(import image img)\nrgb-red')).toEqual([
      "Undefined variable 'rgb-red'",
    ])
  })

  test('a reference through an unknown alias is reported', async () => {
    expect(await scopeErrors('img.rgb-red')).toEqual([
      "No imported module is qualified as 'img'",
    ])
  })

  test('a reference to a name the module does not export is reported', async () => {
    expect(await scopeErrors('(import image img)\nimg.not-a-thing')).toEqual([
      "Module 'img' (image) has no exported binding 'not-a-thing'",
    ])
  })

  test('the one-argument import form still injects names into global scope', async () => {
    expect(await scopeErrors('(import image)\nrgb-red')).toEqual([])
  })

  test('an alias and a same-named define coexist (separate namespaces)', async () => {
    // `img` names the module for `img.rgb-red`; the bare `img` is the define.
    // Neither is a collision.
    expect(
      await scopeErrors('(import image img)\n(define img 5)\nimg\nimg.rgb-red'),
    ).toEqual([])
  })

  test('re-importing the same module under the same alias is idempotent', async () => {
    mockFS({ 'utils.scm': '(define helper 1)' })
    expect(
      await scopeErrors(
        '(import "utils.scm" u)\n(import "utils.scm" u)\nu.helper',
      ),
    ).toEqual([])
  })

  test('binding one alias to two different modules is a collision', async () => {
    mockFS({ 'a.scm': '(define fromA 1)', 'b.scm': '(define fromB 2)' })
    expect(
      await scopeErrors('(import "a.scm" m)\n(import "b.scm" m)'),
    ).toEqual(["Qualified name 'm' is already bound to module 'a.scm'"])
  })

  test('a qualified file import exposes its exports only as alias.member', async () => {
    mockFS({ 'utils.scm': '(define helper 1)\n(define other 2)' })
    expect(await scopeErrors('(import "utils.scm" u)\nu.helper\nu.other')).toEqual(
      [],
    )
    expect(await scopeErrors('(import "utils.scm" u)\nhelper')).toEqual([
      "Undefined variable 'helper'",
    ])
    expect(await scopeErrors('(import "utils.scm" u)\nu.missing')).toEqual([
      "Module 'u' (utils.scm) has no exported binding 'missing'",
    ])
  })

  test('a qualified import of an unknown builtin is still reported', async () => {
    expect(await scopeErrors('(import not-a-lib nl)')).toEqual([
      "No such built-in library: 'not-a-lib'",
    ])
  })
})

describe('qualified imports: runtime', () => {
  test('a qualified reference resolves to the aliased module member', async () => {
    expect(
      await runProgram('(import image img)\n(img.rgb-red (img.rgb 10 20 30))'),
    ).toEqual(['10'])
  })

  test('an unqualified reference after a qualified import fails at runtime', async () => {
    const out = await runProgram('(import image img)\nrgb-red', {
      stripRanges: true,
    })
    expect(out.length).toBe(1)
    expect(out[0]).toContain('rgb-red')
    expect(out[0]).toContain('not found')
  })

  test('the one-argument import form still injects names at runtime', async () => {
    expect(
      await runProgram('(import image)\n(rgb-red (rgb 10 20 30))'),
    ).toEqual(['10'])
  })

  test('an alias and a same-named define resolve independently at runtime', async () => {
    expect(
      await runProgram(
        '(import image img)\n(define img 5)\nimg\n(img.rgb-red (img.rgb 10 20 30))',
      ),
    ).toEqual(['5', '10'])
  })
})
