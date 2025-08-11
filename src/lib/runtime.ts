import { Library, emptyLibrary, registerValue } from '../lang.js'
import * as R from '../lpm/runtime.js'

export const Runtime: Library = emptyLibrary()

/**
 * @returns a predicate function for struct types t.
 */
export function mkPredFn (t: string): (v: R.Value) => boolean {
  return (v: R.Value) => {
    return R.isStructKind(v, t)
  }
}
registerValue('##mkPredFn##', mkPredFn, Runtime)

/**
 * @returns a constructor function for struct type t with the given field names.
 */
export function mkCtorFn (t: string, fieldNames: string[]): (...args: R.Value[]) => R.Struct {
  return (...args: R.Value[]) => {
    if (args.length !== fieldNames.length) {
      throw new R.ScamperError('Runtime', `Constructor ${t} expects ${fieldNames.length} arguments, received ${args.length}`)
    }
    return R.mkStruct(t, fieldNames, args)
  }
}
registerValue('##mkCtorFn##', mkCtorFn, Runtime)

/**
 * @return field accessor function for struct type t and field name f.
 */
export function mkGetFn (t: string, f: string): (v: R.Value) => R.Value {
  return (v: R.Value) => {
    if (R.isStructKind(v, t)) {
      if (!(f in v)) {
        throw new R.ScamperError('Runtime', `Accessor expects field ${f} but it is not present in the given struct value`)
      }
      return v[f]
    } else {
      throw new R.ScamperError('Runtime', `Accessor function expects a ${t}, received ${R.typeOf(v)}`)
    }
  }
}
registerValue('##mkGetFn##', mkGetFn, Runtime)