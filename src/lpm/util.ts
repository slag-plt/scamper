import { ScamperError } from './error.js'
import * as L from './lang.js'


///// Predicates /////////////////////////////////////////////////////////////////

export const isNumber = (v: L.Value): v is number => typeof v === 'number'
export const isBoolean = (v: L.Value): v is boolean => typeof v === 'boolean'
export const isString = (v: L.Value): v is string => typeof v === 'string'
export const isSym = (v: L.Value): v is symbol => typeof v == 'symbol'
export const isSymName = (v: L.Value, name: string): boolean => isSym(v) && Symbol.keyFor(v) === name
export const isNull = (v: L.Value): v is null => v === null
export const isVoid = (v: L.Value): v is undefined => v === undefined
export const isArray = (v: L.Value): v is Array<L.Value> => Array.isArray(v)
export const isTaggedObject = (v: L.Value): v is L.TaggedObject =>
  v !== null && typeof v === 'object' && v.hasOwnProperty(L.scamperTag)
export const isJsFunction = (v: L.Value): v is Function => typeof v === 'function'
export const isClosure = (v: L.Value): v is L.Closure => isTaggedObject(v) && v[L.scamperTag] === 'closure'
export const isFunction = (v: L.Value): v is L.ScamperFn => isJsFunction(v) || isClosure(v)
export const isChar = (v: L.Value): v is L.Char => isTaggedObject(v) && v[L.scamperTag] === 'char'
export const isStruct = (v: L.Value): v is L.Struct => isTaggedObject(v) && v[L.scamperTag] === 'struct'
export const isStructKind = <T extends L.Struct> (v: L.Value, k: string): v is T => isStruct(v) && v[L.structKind] === k

///// Constructors /////////////////////////////////////////////////////////////

export const mkClosure = (params: L.Id[], code: L.Exp, env: L.Env, call: (...args: any) => any, name?: L.Id): L.Closure =>
  ({ [L.scamperTag]: 'closure', params, code, env, call, name })
export const mkChar = (v: string): L.Char => ({ [L.scamperTag]: 'char', value: v })
export const mkSym = (v: string): symbol => Symbol(v)
export const mkStruct = (kind: string, fields: string[], values: L.Value[]): L.Struct => {
  const ret: L.Struct = { [L.scamperTag]: 'struct', [L.structKind]: kind }
  for (let i = 0; i < fields.length; i++) {
    ret[fields[i]] = values[i]
  }
  return ret
}

///// Utility Functions ////////////////////////////////////////////////////////

/** @return true iff the given field name is a hidden field of a struct. */
export function isHiddenField (fld: string): boolean {
  return fld.startsWith('##') && fld.endsWith('##')
}

/** @return a list of the fields of the given struct. */
export function getFieldsOfStruct (s: L.Struct): string[] {
  const ret: string[] = []
  for (const f in s) {
    if (!isHiddenField(f)) {
      ret.push(f)
    }
  }
  return ret
}

/** Mutates a Javascript function to contain a `name` field with that function's name. */
export const nameFn = (name: string, fn: Function): Function =>
  Object.defineProperty(fn, 'name', { value: name })

/**
 * @return true if the two L.Values are structurally equal to each other.
 */
export function equals (v: L.Value, u: L.Value): boolean {
  // N.B., performing a strict equality check covers atomic L.Values and pointer
  // equality without the need for excessive identity checks. We reserve the
  // identity checks for our aggregate L.Values.
  if (v === u) {
    return true
  } else if (isArray(v) && isArray(u)) {
    if (v.length !== u.length) {
      return false
    }
    for (let i = 0; i < v.length; i++) {
      if (!equals(v[i], u[i])) {
        return false
      }
    }
    return true
  } else if (isChar(v) && isChar(u)) {
    return v.value === u.value
  } else if (isStruct(v) && isStruct(u)) {
    if (v[L.structKind] !== u[L.structKind]) {
      return false
    }
    const vFields = getFieldsOfStruct(v)
    const uFields = getFieldsOfStruct(u)
    if (vFields.length !== uFields.length) {
      return false
    }
    for (const f of vFields) {
      if (!equals(v[f], u[f])) {
        return false
      }
    }
    return true
  } else {
    return false
  }
}

/**
 * @returns the type of the given value as a string (for debugging purposes).
 */
export function typeOf (v: L.Value): string {
  if (isNumber(v)) {
    return 'number'
  } else if (isBoolean(v)) {
    return 'boolean'
  } else if (isString(v)) {
    return 'string'
  } else if (isNull(v)) {
    return 'null'
  } else if (isVoid(v)) {
    return 'void'
  } else if (isArray(v)) {
    return 'vector'
  } else if (isJsFunction(v)) {
    return `[Function: ${(v as any).name}]`
  } else if (isClosure(v)) {
    return `[Function: ${v.name ?? '##anonymous##'}]`
  } else if (isChar(v)) {
    return `char`
  } else if (isSym(v)) {
    return `symbol`
  } else if (isPair(v)) {
    return `pair`
  } else if (isList(v)) {
    return `list`
  } else if (isStruct(v)) {
    return `[Struct: ${v[L.structKind]}]`
  } else {
    return typeof v
  }
}

/** @return a generic string representation of value v. */
export function toString (v: L.Value) {
  if (isClosure(v)) {
    return `<closure (${(v as L.Closure).params.join(' ')})>`
  } else if (typeof v === 'function') {
    return `<jsfunc: (${v.name})>`
  } else {
    return `${String(v)}`
  }
}

///// Pairs and Lists //////////////////////////////////////////////////////////

// N.B., We follow Clojure's lead and distinguish between pairs and lists
// explicitly. While they are defined as algebraic datatypes, pairs and lists
// are common enough that are "built-in" datatypes to the runtime.

/**
 * A pair is an algebraic datatype with a first and second component.
 */
export interface Pair extends L.Struct {
  [L.scamperTag]: 'struct',
  [L.structKind]: 'pair',
  fst: L.Value,
  snd: L.Value
}

/**
 * A (non-empty) cons cell is an algebraic datatype representing a non-empty list
 * with a head and tail. The tail, itself, must be a list.
 */
export interface Cons extends L.Struct {
  [L.scamperTag]: 'struct',
  [L.structKind]: 'cons',
  head: L.Value,
  tail: L.Value
}

/** A list is either empty (null) or non-empty (cons) */
export type List = null | Cons

export const isPair = (v: L.Value): v is Pair => isStructKind<Pair>(v, 'pair')
export const isList = (v: L.Value): v is List => v === null || isStructKind<Cons>(v, 'cons')

export const mkPair = (fst: L.Value, snd: L.Value): Pair => ({
  [L.scamperTag]: 'struct',
  [L.structKind]: 'pair',
  fst,
  snd
})

export const mkCons = (head: L.Value, tail: List): Cons => {
  if (!isList(tail)) {
    throw new ScamperError('Runtime', 'The second argument to cons should be a list')
  } else {
    return {
      [L.scamperTag]: 'struct',
      [L.structKind]: 'cons',
      head,
      tail
    }
  }
}

/** @return a vector (array) representation of the input list. */
export function listToVector (l: List): L.Value[] {
  const ret: L.Value[] = []
  let cur = l
  while (cur !== null) {
    ret.push(cur.fst)
    cur = cur.snd as List
  }
  return ret
}

/** @return a list representation of the input vector (array). */
export function vectorToList (arr: L.Value[]): List {
  let ret: List = null
  for (let i = arr.length - 1; i >= 0; i--) {
    ret = mkCons(arr[i], ret)
  }
  return ret
}

export const mkList = (...values: L.Value[]): List => vectorToList(values)

/** @returns the nth element of the list */
export function listNth (n: number, l: List): L.Value {
  if (n < 0) {
    throw new ScamperError('Runtime', `Cannot access negative index ${n} in list`)
  }
  let cur = l
  for (let i = 0; i < n; i++) {
    if (cur === null) {
      throw new ScamperError('Runtime', `List index out of bounds: ${n}`)
    }
    cur = cur.snd as List
  }
  if (cur == null) {
    throw new ScamperError('Runtime', `List index out of bounds: ${n}`)
  } else {
    return cur.fst
  }
}