import { expect, test } from 'vitest'
import * as A from '../../src/scheme/ast.js'
import { tokenizeAndParse } from '../../src/scheme/index.js'
import { librarySources } from '../../src/lib/generated/sources.js'

// https://github.com/slag-plt/scamper/issues/553
//
// A library definition that names a sibling at top level reaches the value
// *behind* its contract wrapper (see VarHandler, #476), so the docstring
// contract -- which is the only type check most natives have -- does not run
// on that call. That set of natives is the bypass surface, and it is what this
// test pins.
//
// The failure mode is not a native that is unguarded today; the other ~300 are
// recorded as unguarded on purpose. It is someone adding a *new*
// library-internal call to one of them and believing the docstring covers it.
// Computing the surface and comparing it against the table below turns that
// into a test failure that says which native, and asks for a guard or a reason.

/** The sub-expressions of `e`, for the walk below. */
function subExps(e: A.Exp): A.Exp[] {
  switch (e.tag) {
    case 'lit': case 'id': case 'hole': return []
    case 'app': return [e.head, ...e.args]
    case 'lam': case 'anonfn': return [e.body]
    case 'let': return [...e.bindings.map((b) => b.value), e.body]
    case 'begin': case 'and': case 'or': case 'vec': return e.exps
    case 'if': return [e.guard, e.ifB, e.elseB]
    case 'match': return [e.scrutinee, ...e.branches.map((b) => b.body)]
    case 'cond': return e.branches.flatMap((b) => [b.test, b.body])
    case 'obj': return e.pairs.flatMap((p) => [p.key, p.value])
  }
}

/** The names `e` applies in operator position, at any depth. */
function appliedNames(e: A.Exp): string[] {
  const here = e.tag === 'app' && e.head.tag === 'id' ? [e.head.name] : []
  return [...here, ...subExps(e).flatMap(appliedNames)]
}

/** The native `(js-var "...")` binds, if `value` is exactly that form. */
function jsVarOf(value: A.Exp): string | undefined {
  const arg = value.tag === 'app' && value.head.tag === 'id'
    && value.head.name === 'js-var' && value.args.length === 1
    ? value.args[0]
    : undefined
  return arg?.tag === 'lit' && typeof arg.value === 'string' ? arg.value : undefined
}

/**
 * Every native reachable without its contract: for each library, the natives
 * its *Scheme-level* definitions (the ones not bound straight to a `js-var`)
 * apply by their top-level names.
 *
 * @returns native name -> the `module:definition` sites that reach it.
 */
function bypassSurface(): Map<string, string[]> {
  const surface = new Map<string, string[]>()
  for (const [mod, src] of librarySources) {
    const prog = tokenizeAndParse(src, undefined, { allowInternalNames: true }).program ?? []
    const defines = prog.filter(
      (s): s is A.Define | A.DefineExport => s.tag === 'define' || s.tag === 'defexport',
    )
    const natives = new Map(
      defines.flatMap((d) => {
        const native = jsVarOf(d.value)
        return native === undefined ? [] : [[d.name.name, native] as const]
      }),
    )
    for (const d of defines) {
      if (jsVarOf(d.value) !== undefined) { continue }
      for (const name of appliedNames(d.value)) {
        const native = natives.get(name)
        if (native === undefined) { continue }
        const sites = surface.get(native) ?? []
        surface.set(native, [...sites, `${mod}:${d.name.name}`])
      }
    }
  }
  return surface
}

/**
 * The surface as it stands, each native with why a call reaching it without a
 * contract is safe. "guarded" means the native re-narrows its own arguments
 * and raises a ScamperError naming itself; anything else is a reason it needs
 * no guard. A native missing from here fails the test below.
 */
const KNOWN_SURFACE: Record<string, string> = {
  canvas_canvasHeight: 'guarded',
  canvas_canvasToPixels: 'guarded',
  canvas_canvasWidth: 'guarded',
  canvas_pixelsQ: 'total: the argument is any -- it is the `pixels?` predicate itself',
  canvas_pixelsToCanvas: 'guarded',
  prelude_apply: 'a bytecode closure rather than a Javascript native: it has no arguments of its own to read',
  prelude_car: 'guarded',
  prelude_cdr: 'guarded',
  prelude_cons: 'total: both arguments are any',
  prelude_error: 'guarded',
  prelude_gt: 'guarded',
  prelude_leq: 'guarded',
  prelude_length: 'guarded',
  prelude_listQ: 'total: the argument is any',
  prelude_listTail: 'guarded: it backs both list-tail and list-drop, which sort reaches (#649)',
  prelude_listTake: 'guarded',
  prelude_listToString: 'guarded: it raises on any element that is not a char, which a non-list trips at once',
  prelude_listToVector: 'guarded',
  prelude_lt: 'guarded',
  prelude_makeVector: 'guarded',
  prelude_minus: 'guarded',
  prelude_nullQ: 'total: the argument is any',
  prelude_plus: 'guarded',
  prelude_quotient: 'guarded',
  prelude_reverse: 'guarded',
  prelude_stringToList: 'guarded',
  prelude_vectorLength: 'guarded',
  prelude_vectorRef: 'guarded',
  prelude_vectorSet: 'guarded',
  prelude_vectorToList: 'guarded',
  test_testResultErrorExn: 'guarded',
  test_testResultErrorExpected: 'guarded',
  test_testResultErrorGeneric: 'guarded',
  test_testResultOk: 'guarded',
}

test('#553: a library-internal call to an unguarded native is accounted for', () => {
  // The failure carries the native *and* the definition that reached it, so
  // the fix -- guard the native, or add a line above saying why it needs none
  // -- is the obvious next step rather than a puzzle.
  expect(
    [...bypassSurface().entries()]
      .filter(([native]) => !(native in KNOWN_SURFACE))
      .map(([native, sites]) => `${native}, reached from ${sites.join(', ')}`)
      .sort(),
  ).toEqual([])
})

test('#553: the table above holds no native library code has stopped reaching', () => {
  const surface = bypassSurface()
  expect(Object.keys(KNOWN_SURFACE).filter((n) => !surface.has(n))).toEqual([])
})

test('#553: the surface is computed, not hand-listed', () => {
  // A sanity check on the walk itself: `pixel-map` is a Scheme-level define
  // whose body applies four natives by their top-level names, which is the
  // shape the whole test depends on recognising.
  expect(bypassSurface().get('canvas_pixelsToCanvas')).toEqual(['image:pixel-map'])
})
