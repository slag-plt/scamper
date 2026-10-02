import { afterEach, describe, expect, test, vi } from 'vitest'
import * as FS from '../../src/fs'
import builtinLibs, { docRegistry } from '../../src/lib'
import { Module } from '../../src/lpm/lang'
import { scopeCheckProgram } from '../../src/scheme/scope'
import { expandProgram } from '../../src/scheme/expansion'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'
import * as SymbolDB from '../../src/scheme/symbol-db'
import { MockFileSystem } from '../stubs/mock-file-system'

// Regression test for #682.
//
// Some names are exported by more than one builtin library: `canvas` and
// `image` both export `canvas?`, `color?`, `drawing?`, `fill-mode?` and
// `font?`; `html` and `reactive` both export `button?`; `image`, `lab` and
// `reactive` all export `html?`. Each is a deliberate re-export of one and the
// same native, because a contract predicate has to resolve in the module whose
// own contracts name it (see the N.B.s in src/lib/canvas.scm).
//
// Importing two of them therefore brought one name in from two modules, which
// the scope checker reported as "Global variable 'canvas?' is already
// defined". The warning was spurious: the program ran correctly in either
// import order, because whichever binding won guarded the same native.
//
// A name arriving from two modules is now a collision only when the two are not
// both libraries exporting it as the same procedure. Each library wraps its own
// exports in contracts built from its own docstrings, so the two wrappers are
// never the same object; SymbolDB.exportSameBinding compares what they guard.

async function scopeErrors(src: string): Promise<string[]> {
  const errors: ScamperDiagnostic[] = []
  const parseErrs: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(parseErrs, src)
  expect(parseErrs, 'test source should parse cleanly').toEqual([])
  await scopeCheckProgram(errors, expandProgram(prog))
  return errors.map((e) => e.message)
}

/** As `scopeErrors`, with `files` visible on a mock file system. */
async function scopeErrorsWithFiles(
  files: Record<string, string>,
  src: string,
): Promise<string[]> {
  const fs = new MockFileSystem()
  for (const [name, contents] of Object.entries(files)) {
    await fs.saveFile(name, contents)
  }
  vi.spyOn(FS, 'getFS').mockReturnValue(fs)
  return await scopeErrors(src)
}

afterEach(() => {
  vi.restoreAllMocks()
})

/** Every builtin library name, in load order. */
const libs = [...builtinLibs.keys()]

describe('#682: libraries re-exporting one native do not collide', () => {
  test('the reported pairings are clean', async () => {
    expect(await scopeErrors('(import canvas)\n(import image)')).toEqual([])
    expect(await scopeErrors('(import html)\n(import reactive)')).toEqual([])
    expect(await scopeErrors('(import image)\n(import lab)')).toEqual([])
    expect(await scopeErrors('(import image)\n(import reactive)')).toEqual([])
    expect(await scopeErrors('(import lab)\n(import reactive)')).toEqual([])
  })

  test('order does not matter', async () => {
    expect(await scopeErrors('(import image)\n(import canvas)')).toEqual([])
    expect(await scopeErrors('(import reactive)\n(import html)')).toEqual([])
  })

  test('three libraries sharing one name are clean', async () => {
    // html? comes from image, lab and reactive alike. The third import is
    // compared against the *value* the first registered, not its module name.
    expect(
      await scopeErrors('(import image)\n(import lab)\n(import reactive)'),
    ).toEqual([])
  })

  test('no pair of builtin libraries collides at all', async () => {
    // The strengthening of #663's sweep, which had to filter on js-var because
    // these pairings were still outstanding. Every one of the 91 pairs is now
    // silent -- and this is what would flag a *new* library exporting an
    // existing name with a different meaning.
    const offenders: string[] = []
    for (const [i, a] of libs.entries()) {
      for (const b of libs.slice(i + 1)) {
        offenders.push(
          ...(await scopeErrors(`(import ${a})\n(import ${b})`)).map(
            (m) => `(import ${a}) + (import ${b}): ${m}`,
          ),
        )
      }
    }
    expect(offenders).toEqual([])
  })
})

describe('#682: a genuine collision is still reported', () => {
  test('a define and an import of one name, either order', async () => {
    expect(
      await scopeErrors('(define html? (lambda (v) #t))\n(import image)'),
    ).toEqual(["Global variable 'html?' is already defined"])
    expect(
      await scopeErrors('(import image)\n(define html? (lambda (v) #t))'),
    ).toEqual(["Global variable 'html?' is already defined"])
  })

  test('two files exporting one name', async () => {
    // Only builtin libraries are compared by value: a file module has not been
    // run when the importing program is checked, so there is nothing to compare
    // and the conservative answer -- report it -- stands.
    expect(
      await scopeErrorsWithFiles(
        {
          'a.scm': '(define-export helper 1)',
          'b.scm': '(define-export helper 2)',
        },
        '(import "a.scm")\n(import "b.scm")',
      ),
    ).toEqual(["Global variable 'helper' is already defined"])
  })

  test('a file named after a library shadowing one of its names', async () => {
    // The two namespaces are separate: a file called `canvas` is not the
    // `canvas` library, however alike the names look. Its `canvas?` genuinely
    // shadows the library's -- at runtime the later import wins -- so both
    // orders warn, and `exportSameBinding` must never be consulted here.
    const shadow = '(define-export canvas? (lambda (v) #t))'
    expect(
      await scopeErrorsWithFiles(
        { canvas: shadow },
        '(import "canvas")\n(import image)',
      ),
    ).toEqual(["Global variable 'canvas?' is already defined"])
    expect(
      await scopeErrorsWithFiles(
        { canvas: shadow },
        '(import image)\n(import "canvas")',
      ),
    ).toEqual(["Global variable 'canvas?' is already defined"])
  })

  test('a file named after the library it collides with', async () => {
    // The same conflation seen from the other side, and a bug that predates
    // #682: the file and the library were taken for one module re-imported, so
    // the collision was skipped as idempotent and nothing was reported at all.
    const errs = await scopeErrorsWithFiles(
      { canvas: '(define-export canvas? (lambda (v) #t))' },
      '(import "canvas")\n(import canvas)',
    )
    expect(errs).toEqual(["Global variable 'canvas?' is already defined"])
  })

  test('re-importing one library really is idempotent', async () => {
    expect(await scopeErrors('(import image)\n(import image)')).toEqual([])
    expect(
      await scopeErrorsWithFiles(
        { 'a.scm': '(define-export helper 1)' },
        '(import "a.scm")\n(import "a.scm")',
      ),
    ).toEqual([])
  })
})

describe('#682: exportSameBinding', () => {
  /** Registers throwaway libraries for the body, then removes them. */
  function withLibs(
    mods: Record<string, Record<string, unknown>>,
    body: () => void,
  ): void {
    for (const [libName, bindings] of Object.entries(mods)) {
      const mod = new Module()
      for (const [name, value] of Object.entries(bindings)) {
        mod.registerValue(name, value as never)
      }
      builtinLibs.set(libName, mod)
    }
    try {
      body()
    } finally {
      Object.keys(mods).forEach((libName) => builtinLibs.delete(libName))
    }
  }

  test('two libraries re-exporting one native share a binding', () => {
    // Each of these is two distinct contract wrappers around one native, so
    // these assertions are exactly what pins the unwrapping: compare the
    // exported values directly and every one of them is false.
    expect(SymbolDB.exportSameBinding('canvas', 'image', 'canvas?')).toBe(true)
    expect(SymbolDB.exportSameBinding('html', 'reactive', 'button?')).toBe(true)
    expect(SymbolDB.exportSameBinding('lab', 'reactive', 'html?')).toBe(true)
    expect(
      builtinLibs.get('canvas')?.bindings.get('canvas?') ===
        builtinLibs.get('image')?.bindings.get('canvas?'),
    ).toBe(false)
  })

  test('a name only one of them exports does not', () => {
    expect(SymbolDB.exportSameBinding('canvas', 'image', 'make-canvas')).toBe(
      false,
    )
    expect(SymbolDB.exportSameBinding('canvas', 'image', 'no-such-name')).toBe(
      false,
    )
  })

  test('a module that is not a builtin library never matches', () => {
    expect(SymbolDB.exportSameBinding('canvas', 'a.scm', 'canvas?')).toBe(false)
    expect(SymbolDB.exportSameBinding('a.scm', 'b.scm', 'helper')).toBe(false)
  })

  test('one name, two different procedures, is two bindings', () => {
    withLibs(
      {
        'probe-a': { probe: () => 1 },
        'probe-b': { probe: () => 2 },
      },
      () => {
        expect(SymbolDB.exportSameBinding('probe-a', 'probe-b', 'probe')).toBe(
          false,
        )
      },
    )
  })

  test('one shared procedure, reached through either library, is one binding', () => {
    const shared = (): number => 1
    withLibs({ 'probe-a': { probe: shared }, 'probe-b': { probe: shared } }, () => {
      expect(SymbolDB.exportSameBinding('probe-a', 'probe-b', 'probe')).toBe(true)
    })
  })

  test('equal non-procedures are two bindings, not one', () => {
    // Two libraries each defining a constant that happens to equal the other's
    // are two bindings, and one does shadow the other. Only a shared *procedure*
    // is the re-export this exists for.
    withLibs({ 'probe-a': { probe: 100 }, 'probe-b': { probe: 100 } }, () => {
      expect(SymbolDB.exportSameBinding('probe-a', 'probe-b', 'probe')).toBe(
        false,
      )
    })
  })
})

describe('#682: what makes the suppression safe', () => {
  /** The names more than one builtin library exports, and who exports them. */
  function sharedNames(): [string, string[]][] {
    const owners = new Map<string, string[]>()
    for (const lib of libs) {
      for (const name of builtinLibs.get(lib)?.bindings.keys() ?? []) {
        owners.set(name, [...(owners.get(name) ?? []), lib])
      }
    }
    return [...owners].filter(([, mods]) => mods.length > 1)
  }

  test('every name two libraries share is one procedure in both', () => {
    // The invariant the fix rests on. A library that starts exporting an
    // existing name with a *different* value breaks this -- and should, so that
    // the collision is reported rather than silently merged.
    const shared = sharedNames()
    expect(shared.length, 'the cases this test exists for').toBeGreaterThan(0)
    const disagreements = shared.filter(([name, mods]) =>
      mods.some((m) => !SymbolDB.exportSameBinding(mods[0], m, name)),
    )
    expect(disagreements.map(([name]) => name)).toEqual([])
  })

  test('their contracts agree, so which binding wins is unobservable', () => {
    // What `exportSameBinding` compares is the native behind the wrappers; the
    // wrappers themselves are still shadowed, and the last import is the one a
    // student gets (Env.lookup takes imports most-recent-first). That is
    // invisible only while the two docstrings generate the same checks. This is
    // what pins it: a divergent signature for a shared name shows up here.
    const differing: string[] = []
    for (const [name, mods] of sharedNames()) {
      const contracts = mods.map((m) =>
        JSON.stringify(contractOf(docRegistry.get(m)?.get(name))),
      )
      if (new Set(contracts).size > 1) {
        differing.push(`${name}: ${mods.join(' vs ')}`)
      }
    }
    expect(differing).toEqual([])
  })
})

/**
 * A documented function's contract-relevant parts -- its signature and
 * parameters, with source ranges and prose dropped, since those differ freely
 * between two libraries documenting one function.
 */
function contractOf(doc: unknown): unknown {
  const strip = (v: unknown): unknown => {
    if (Array.isArray(v)) {
      return v.map(strip)
    }
    if (v !== null && typeof v === 'object') {
      return Object.fromEntries(
        Object.entries(v)
          .filter(([k]) => k !== 'range' && k !== 'description')
          .map(([k, x]) => [k, strip(x)]),
      )
    }
    return v
  }
  if (doc === undefined || doc === null || typeof doc !== 'object') {
    return doc
  }
  const { signature, params, optParams, restParam } = doc as Record<
    string,
    unknown
  >
  return strip({ signature, params, optParams, restParam })
}
