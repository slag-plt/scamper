import { Value } from '../lpm/runtime.ts'
import * as R from '../lpm/runtime.ts'

export function isAtom (v: Value): boolean {
  return !R.isList(v)
}

export function isSexp (v: Value): boolean {
  return R.isList(v)
}