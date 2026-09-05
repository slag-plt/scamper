import { expect, test } from 'vitest'
import { builtinLibs, docRegistry } from '../../src/lib'
import { librarySources } from '../../src/lib/generated/sources'
import * as L from '../../src/lpm'
import { FunctionDoc } from '../../src/scheme/docstring/docstring'
import { functionDocSignature } from '../../src/scheme/docstring/render'

// A library docstring is not just documentation. src/lib/index.ts compiles
// every module with `insertContracts: true`, so contractStmt turns each
// signature line into the lambda that *stands in front of* the implementation:
// a docstring's arity IS the binding's runtime arity, and nothing checked it
// against the thing it wraps.
//
// For a Javascript native the disagreement is silent -- applyFn calls
// `fn(...args)` with no arity check, so a missing argument arrives as
// `undefined` and an extra one is dropped. That has shipped three times
// (#455, #467, #492), which is why this is one generic sweep over every
// documented binding rather than a test per library function (#496).
//
// Deliberately arity only: whether a documented *predicate* matches what the
// implementation accepts is #495's question, and asking it here would bury
// these failures under false positives.

/**
 * Bindings whose docstring is known to disagree with its implementation, and
 * why. A new entry needs a reason and an issue -- it means the signature a
 * student reads is not the one the binding has.
 */
const KNOWN_BAD = new Map<string, string>([
  [
    'prelude:??',
    // `??: any` documents a constant, but prelude_qq is a nullary function
    // that throws "Hole encountered in program!". Since the binding is that
    // function, `(+ 1 ??)` reports a type error instead of the hole. Fixing it
    // decides what a hole *is* -- a language question, not a docstring edit.
    'documented as a constant but bound to a nullary function -- see #537',
  ],
])

/** How many arguments an implementation takes, and nothing else about it. */
interface Arity {
  /** The fewest it can be called with. */
  required: number
  /** The most, not counting a rest parameter. */
  total: number
  /** Whether it collects further arguments into a rest parameter. */
  rest: boolean
}

/**
 * The parameters of a Javascript function's source, split on the top-level
 * commas of its parameter list. Bracket depth and quotes are tracked so a
 * destructured or defaulted parameter stays one piece, and only *structure* is
 * ever read -- never a name, which a transform is free to rewrite.
 */
function parseParams(src: string): string[] {
  // A parenless arrow (`x => x`) has one parameter and no parentheses to find.
  const parenless = /^(?:async\s+)?([A-Za-z_$][\w$]*)\s*=>/.exec(src.trim())
  if (parenless !== null) {
    return [parenless[1]]
  }
  const open = src.indexOf('(')
  if (open === -1) {
    throw new Error(`no parameter list in: ${src.slice(0, 80)}`)
  }
  const params: string[] = []
  let current = ''
  let depth = 0
  let quote: string | undefined
  for (let i = open; i < src.length; i++) {
    const c = src[i]
    if (quote !== undefined) {
      if (c === '\\') {
        current += c + (src[i + 1] ?? '')
        i++
        continue
      }
      if (c === quote) {
        quote = undefined
      }
    } else if (c === '"' || c === "'" || c === '`') {
      quote = c
    } else if ('([{'.includes(c)) {
      depth++
      if (depth === 1) {
        // The opening parenthesis of the list itself, not part of a parameter.
        continue
      }
    } else if (')]}'.includes(c)) {
      depth--
      if (depth === 0) {
        return [...params, current]
          .map((p) => p.trim())
          .filter((p) => p.length > 0)
      }
    } else if (c === ',' && depth === 1) {
      params.push(current)
      current = ''
      continue
    }
    current += c
  }
  throw new Error(`unterminated parameter list in: ${src.slice(0, 80)}`)
}

/** Whether a parameter declares a default value, i.e. holds a top-level `=`. */
function hasDefault(param: string): boolean {
  let depth = 0
  for (let i = 0; i < param.length; i++) {
    const c = param[i]
    if ('([{'.includes(c)) {
      depth++
    } else if (')]}'.includes(c)) {
      depth--
    } else if (
      c === '=' &&
      depth === 0 &&
      param[i + 1] !== '=' &&
      !['=', '!', '<', '>'].includes(param[i - 1])
    ) {
      return true
    }
  }
  return false
}

/**
 * A Javascript function's arity. `fn.length` counts only the parameters before
 * the first default or rest, so it cannot see either on its own and the
 * parameter list has to be parsed. The parse is then cross-checked against
 * `fn.length`, which turns any future drift in this parser -- or in the
 * transform feeding it -- into a loud failure rather than a silent pass.
 */
function jsArity(fn: L.JsFunction): Arity {
  const params = parseParams(fn.toString())
  const rest = params.at(-1)?.startsWith('...') ?? false
  const fixed = rest ? params.slice(0, -1) : params
  const firstDefault = fixed.findIndex(hasDefault)
  const required = firstDefault === -1 ? fixed.length : firstDefault
  if (fn.length !== required) {
    // Either this parser has drifted from the source it reads, or the binding
    // has no parameter list in its source to read: a built-in (`Math.floor`)
    // or a bound function prints as `[native code]`, and would need its arity
    // recorded some other way.
    throw new Error(
      `parsed ${String(required)} required parameter(s) but Function.length says ${String(fn.length)} -- this parser has drifted, or the function's source is not readable (e.g. "[native code]"): ${fn.toString().slice(0, 120)}`,
    )
  }
  return { required, total: fixed.length, rest }
}

/** A Scamper closure's arity, read straight off its parameter list. */
function closureArity(c: L.Closure): Arity {
  return {
    required: c.params.length,
    total: c.params.length,
    rest: c.restParam !== undefined,
  }
}

/** `3` -> `"3 arguments"`, `1` -> `"1 argument"`. */
function args(n: number): string {
  return `${String(n)} argument${n === 1 ? '' : 's'}`
}

/** Describes an implementation's arity the way a failure message wants it. */
function describeArity(a: Arity): string {
  if (a.rest) {
    return `takes ${args(a.required)} and any number more`
  }
  return a.required === a.total
    ? `takes exactly ${args(a.total)}`
    : `takes ${String(a.required)} required, ${args(a.total)} at most`
}

/**
 * How many arguments the contract wrapper can hand the implementation. An
 * optional parameter is always passed positionally -- void when the caller
 * omits it -- so it raises the floor as well as the ceiling: contractStmt
 * calls `mkTargetCall([...params, ...optParams], restParam, ...)`, which
 * passes every optional whether or not a rest parameter follows them. A rest
 * parameter then lifts the ceiling to infinity, leaving the floor where it is.
 *
 * N.B., no library signature declares optionals *and* a rest parameter today,
 * so that half of the floor is unobservable: it is written from what
 * contract.ts lowers to rather than from a failure it has caught.
 */
function docArity(doc: FunctionDoc): { min: number; max: number } {
  const passed = doc.params.length + doc.optParams.length
  return { min: passed, max: doc.restParam !== undefined ? Infinity : passed }
}

/** Unwraps the contract lambda a docstring lowered to, if there is one. */
function implementationOf(v: L.Value): L.Value {
  return L.isClosure(v) && v.contractTarget !== undefined ? v.contractTarget : v
}

/**
 * Every `<module>:<name>` in the standard library whose documented arity
 * disagrees with its implementation's, mapped to the complaint.
 */
function arityDisagreements(): Map<string, string> {
  const bad = new Map<string, string>()
  for (const [module, docs] of docRegistry) {
    const mod = builtinLibs.get(module)
    // allBindings when the module has private helpers; they are documented too.
    const bindings = mod?.allBindings ?? mod?.bindings
    for (const [name, doc] of docs) {
      const value = implementationOf(bindings?.get(name))
      const signature = functionDocSignature(doc).split('\n')[0]
      const complain = (problem: string): void => {
        bad.set(
          `${module}:${name}`,
          `${module}.scm:${String(doc.range.begin.line)}: ${signature} -- ${problem}`,
        )
      }

      // Rule 4: a constant's signature must not name a function.
      if (doc.signature.isConstant) {
        if (L.isFunction(value)) {
          complain('documented as a constant, but the binding is a function')
        }
        continue
      }
      // Rule 3: a function's signature must name something callable.
      if (!L.isFunction(value)) {
        complain(
          `documented as a function, but the binding is a ${L.typeOf(value)} -- document it as a constant, "${name}: <predicate>"`,
        )
        continue
      }
      const impl = L.isClosure(value) ? closureArity(value) : jsArity(value)
      const { min, max } = docArity(doc)
      if (min < impl.required) {
        // Rule 1: too few -- the implementation is handed `undefined` for
        // something it declares.
        complain(
          `passes as few as ${args(min)}, but the implementation ${describeArity(impl)}`,
        )
      } else if (!impl.rest && max > impl.total) {
        // Rule 2: too many -- the extra arguments are silently dropped.
        complain(
          `passes ${max === Infinity ? 'any number of arguments' : `as many as ${args(max)}`}, but the implementation ${describeArity(impl)}`,
        )
      }
    }
  }
  return bad
}

test('a contracted binding unwraps to its implementation', () => {
  // Without this the sweep could pass vacuously: a registry missing modules
  // narrows it silently rather than failing, and if contract insertion stopped
  // tagging its wrapper with contractTarget every binding would be compared
  // against the wrapper built from its own docstring and agree trivially.
  expect(
    docRegistry.size,
    'every library module should be in the doc registry',
  ).toBe(librarySources.length)
  expect(docRegistry.get('prelude')?.has('substring')).toBe(true)
  const substring = builtinLibs.get('prelude')?.bindings.get('substring')
  expect(L.isClosure(substring), 'substring should be a contract wrapper').toBe(
    true,
  )
  expect(
    typeof implementationOf(substring),
    'substring should unwrap to its Javascript native',
  ).toBe('function')
})

test("every documented signature's arity matches its implementation", () => {
  const bad = arityDisagreements()
  const unexpected = [...bad].filter(([key]) => !KNOWN_BAD.has(key))
  expect(
    unexpected.map(([, message]) => message),
    "a docstring's arity is the binding's runtime arity, so a disagreement is a bug a student hits",
  ).toEqual([])
})

test('the known exceptions still disagree, so they can be retired when fixed', () => {
  const bad = arityDisagreements()
  for (const [key, reason] of KNOWN_BAD) {
    expect(
      bad.get(key),
      `${key} now agrees with its implementation (${reason}) -- drop it from KNOWN_BAD`,
    ).toBeDefined()
  }
})
