import * as L from '../lpm'

export const Runtime: L.Library = new L.Library()

/**
 * @returns a predicate function for struct types t.
 */
export function mkPredFn (t: string): (v: L.Value) => boolean {
  return (v: L.Value) => {
    return L.isStructKind(v, t)
  }
}
Runtime.registerValue('##mkPredFn##', mkPredFn)

/**
 * @returns a constructor function for struct type t with the given field names.
 */
export function mkCtorFn (t: string, fieldNames: string[]): (...args: L.Value[]) => L.Struct {
  return (...args: L.Value[]) => {
    if (args.length !== fieldNames.length) {
      throw new L.ScamperError('Runtime', `Constructor ${t} expects ${fieldNames.length} arguments, received ${args.length}`)
    }
    return L.mkStruct(t, fieldNames, args)
  }
}
Runtime.registerValue('##mkCtorFn##', mkCtorFn)

/**
 * @return field accessor function for struct type t and field name f.
 */
export function mkGetFn (t: string, f: string): (v: L.Value) => L.Value {
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
Runtime.registerValue('##mkGetFn##', mkGetFn)