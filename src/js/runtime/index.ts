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
 * @returns a predicate function for struct types t.
 */
export function runtime_mkPredFn (t: string): (v: L.Value) => boolean {
  return (v: L.Value) => {
    return L.isStructKind(v, t)
  }
}

/**
 * @returns a constructor function for struct type t with the given field names.
 */
export function runtime_mkCtorFn (t: string, fieldNames: string[]): (...args: L.Value[]) => L.Struct {
  return (...args: L.Value[]) => {
    if (args.length !== fieldNames.length) {
      throw new L.ScamperError('Runtime', `Constructor ${t} expects ${fieldNames.length} arguments, received ${args.length}`)
    }
    return L.mkStruct(t, fieldNames, args)
  }
}

/**
 * @return field accessor function for struct type t and field name f.
 */
export function runtime_mkGetFn (t: string, f: string): (v: L.Value) => L.Value {
  return (v: L.Value) => {
    if (L.isStructKind(v, t)) {
      if (!(f in v)) {
        throw new L.ScamperError('Runtime', `Accessor expects field ${f} but it is not present in the given struct value`)
      }
      return v[f]
    } else {
      throw new L.ScamperError('Runtime', `Accessor function expects a ${t}, received ${L.typeOf(v)}`)
    }
  }
}

/**
 * @return the type of the given value as a string
 */
export function runtime_typeOf (v: L.Value): string {
  return L.typeOf(v)
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
