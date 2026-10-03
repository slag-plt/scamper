import * as L from '../../lpm'

/**
 * Aborts the running fiber, reporting `value` as the answer to a
 * live-evaluation query. Bound to the internal `##report##`; a query wraps its
 * target sub-expression in `(##report## <expr>)`. The thrown ReportError
 * bypasses with-handler in the scheduler and is caught by the query runner,
 * which reads its `value`. The range is unused by the consumer (the queried
 * range is computed separately), so Range.none suffices.
 */
export function runtime_report (value: L.Value): L.Value {
  throw new L.ReportError(value, L.Range.none)
}

/**
 * Raises a runtime error carrying `msg`. Bound to the internal `##error##`,
 * which expansion and contract insertion inject by reference -- a `cond`
 * fall-through becomes `(##error## "No matching clause in cond")`. It is
 * internal (rather than the prelude's `error`) so a user binding named `error`
 * cannot change what those forms do.
 *
 * A separate function object from prelude_error, not an alias: Module
 * .registerValue renames whatever it binds, so sharing one object would
 * rename the prelude's `error` too.
 *
 * @param blame the procedure whose promise was broken, reported in place of
 *        `error`. A contract check passes the name of the definition it guards,
 *        so a violation reads `(not) expected a boolean ...` rather than naming
 *        the mechanism (#633). Omitted by a raise that has no such owner -- a
 *        `cond` fall-through, and the prelude's own `error` -- which reports
 *        `error`. That default is what keeps the internal `##error##` spelling
 *        out of a student's error even when they have bound `error` themselves;
 *        see test/regressions/internal-name-hygiene.test.ts.
 */
export const runtime_error: L.JsFunction = L.nameFn('error', (msg: L.Value, blame?: L.Value): L.Value => {
  const source = typeof blame === 'string' ? blame : 'error'
  if (typeof msg !== 'string') {
    throw new L.ScamperError(
      'Runtime',
      `expected a string, received ${L.typeOf(msg)}`,
      undefined,
      undefined,
      source,
    )
  }
  throw new L.ScamperError('Runtime', msg, undefined, undefined, source)
})

/**
 * The optional arguments a contract wrapper was called with, as a list: what
 * the wrapper's own rest parameter collected, minus any it has already bound.
 * See scheme/contract.ts.
 */
type OptArgs = L.List

/** @returns `opts` less its first `n` elements, or null if it is shorter. */
function dropOpts (opts: OptArgs, n: number): L.List {
  let curr = opts
  for (let i = 0; i < n && curr !== null; i++) {
    curr = curr.tail
  }
  return curr
}

/**
 * The `i`th optional argument, or void when the caller stopped short of it.
 * Bound to the internal `##optArg##`.
 */
export function runtime_optArg (opts: OptArgs, i: number): L.Value {
  const at = dropOpts(opts, i)
  return at === null ? undefined : at.head
}

/**
 * Whatever follows the first `n` optional arguments -- the value of a
 * signature's rest parameter, since the wrapper's own rest parameter collected
 * the optionals and the rest together. Bound to the internal `##optRest##`.
 */
export function runtime_optRest (opts: OptArgs, n: number): L.List {
  return dropOpts(opts, n)
}

/**
 * Raises the arity error a fixed-arity function would have raised, for a
 * signature whose optional parameters are its last: anything beyond the first
 * `numOpts` optional arguments is one argument too many. Bound to the internal
 * `##checkArity##`.
 * @param numRequired how many parameters the wrapper's own lambda takes, which
 *        the caller supplied before `opts` begins.
 */
export function runtime_checkArity (
  opts: OptArgs, numOpts: number, numRequired: number
): undefined {
  let extra = dropOpts(opts, numOpts)
  if (extra === null) { return undefined }
  let given = numRequired + numOpts
  while (extra !== null) {
    given += 1
    extra = extra.tail
  }
  // Shares its wording with the closure arity check (L.arityMismatchMsg), which
  // is what keeps "argument" singular in both when the ceiling is one -- no
  // library signature has such a ceiling today, so nothing a student sees moves.
  throw new L.ScamperError(
    'Runtime',
    L.arityMismatchMsg('at most', numRequired + numOpts, given))
}

/**
 * Whether `v` is void, i.e. an optional argument the caller left out. Bound to
 * the internal `##voidQ##`, and a separate binding from the prelude's `void?`
 * so a library parameter of that name cannot change what a contract check
 * means.
 */
export function runtime_voidQ (v: L.Value): boolean {
  return v === undefined
}

/**
 * Each of the three functions a `struct` lowers to is named for its Scamper
 * spelling (`point`, `point?`, `point-x`), as every library native is by
 * Module.registerValue.
 *
 * The two that can fail also *report* themselves under that spelling, setting
 * the raised error's source rather than leaving applyFn to choose one. They
 * have no contract wrapper to speak for them, so applyFn would otherwise name
 * the enclosing library frame -- which for a higher-order call is an internal
 * helper the student never wrote (`(map point-x ...)` blamed prelude's `apply`;
 * #633). applyFn fills the source in only when the thrower left it unset, so
 * naming themselves here carries through any depth of library frames.
 *
 * @returns a predicate function for struct types t.
 */
export function runtime_mkPredFn (t: string): (v: L.Value) => boolean {
  return L.nameFn(`${t}?`, (v: L.Value) => {
    return L.isStructKind(v, t)
  })
}

/**
 * @returns a constructor function for struct type t with the given field names.
 */
export function runtime_mkCtorFn (t: string, fieldNames: string[]): (...args: L.Value[]) => L.Struct {
  return L.nameFn(t, (...args: L.Value[]) => {
    if (args.length !== fieldNames.length) {
      throw new L.ScamperError('Runtime', `Constructor ${t} expects ${fieldNames.length} ${L.argumentNoun(fieldNames.length)}, received ${args.length}`, undefined, undefined, t)
    }
    return L.mkStruct(t, fieldNames, args)
  })
}

/**
 * @return field accessor function for struct type t and field name f.
 */
export function runtime_mkGetFn (t: string, f: string): (v: L.Value) => L.Value {
  const name = `${t}-${f}`
  return L.nameFn(name, (v: L.Value) => {
    if (L.isStructKind(v, t)) {
      if (!(f in v)) {
        throw new L.ScamperError('Runtime', `Accessor expects field ${f} but it is not present in the given struct value`, undefined, undefined, name)
      }
      return v[f]
    } else {
      throw new L.ScamperError('Runtime', `Accessor function expects a ${t}, received ${L.typeOf(v)}`, undefined, undefined, name)
    }
  })
}

/**
 * @return the type of the given value as a string
 */
export function runtime_typeOf (v: L.Value): string {
  return L.typeOf(v)
}

/**
 * Tags `wrapper` -- the checking lambda a docstring's contract lowers to -- with
 * the value it wraps, so the machine can reach that value directly on a call
 * made from library code (see VarHandler). Bound to the internal
 * `##contracted##` and injected by contract insertion; nothing else calls it.
 *
 * @returns a copy of `wrapper` carrying `target`, or `wrapper` unchanged when
 *          it is not a closure (which contract insertion never produces).
 */
export function runtime_contracted (target: L.Value, wrapper: L.Value): L.Value {
  return L.isClosure(wrapper) ? { ...wrapper, contractTarget: target } : wrapper
}

/**
 * @returns #t unconditionally -- the "any" predicate, matching every value.
 */
export function runtime_any (_v: L.Value): boolean {
  return true
}

/**
 * Builds the vector a `[e1 ... ek]` literal denotes. Bound to the internal
 * `##mkVec##` rather than reusing the prelude's `vector` so that a user binding
 * named `vector` cannot change what a vector literal means.
 * @returns a vector of the arguments, in order
 */
export function runtime_mkVec (...args: L.Value[]): L.Value[] {
  return args
}

/**
 * Builds the map (Javascript object) a `{k1 v1 ... kn vn}` literal denotes.
 * Bound to the internal `##mkObj##`; expansion rewrites every map literal into
 * a call to it.
 * @param args alternating keys and values; each key must be a string
 * @returns the object mapping each key to its value
 */
export function runtime_mkObj (...args: L.Value[]): object {
  // The parser rejects an odd element count, so this only fires if something
  // calls ##mkObj## directly.
  if (args.length % 2 !== 0) {
    throw new L.ScamperError('Runtime', `A map requires an even number of arguments (alternating keys and values), received ${args.length.toString()}`)
  }
  const ret: Record<string, L.Value> = {}
  for (let i = 0; i < args.length; i += 2) {
    const key = args[i]
    if (typeof key !== 'string') {
      throw new L.ScamperError('Runtime', `A map key must be a string, received ${L.typeOf(key)}`)
    }
    ret[key] = args[i + 1]
  }
  return ret
}
