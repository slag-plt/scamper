import { Range, noRange } from './range.js'

/***** Foundational Types *****************************************************/

/** The type of identifiers at runtime. */
export type Id = string

/** The type of indices into runtime maps. */
export type Idx = number

/** The type of instruction locations of a (compiled) program. */
export type OpIdx = number

/** The type of variables at runtime, i.e., indices into a local environment. */
export type Var = number

/** (Local) environments map variables to values. */
export type Env = Value[]

/** A (compiled) program is a linear sequence of opcodes. */
export type OpCodes = DataView

/***** Values *****************************************************************/

/** The field name for the tags of tagged objects. */
export const scamperTag = Symbol('tag')
/** The field name for the kind of structs. */
export const structKind = Symbol('kind')

/** A closure captures a function value and its execution environment. */
export type Closure = {
  [scamperTag]: 'closure',
  arity: number,
  params: Id[],
  idx: OpIdx,
  env: Env,
  name?: string
}

/** A char is a string of size one. */
export type Char = { [scamperTag]: 'char', value: string }

/**
 * A pair is a structure that contains two values. The `isList` tag is
 * maintained internally by the runtime to efficiently implement core list
 * operations.
 */
export type Pair = {
  [scamperTag]: 'pair',
  fst: Value,
  snd: Value,
  isList: boolean
}

/** A list is either null or a pair of a value and a list. Note that this
 *  constraint is _not_ enforced at the type level. */
export type List = null | Pair

/** A vector is an array of values. */
export type Vector = Value[]

/** The uninhabited type. */
export type Void = undefined

/** A struct is a discriminated union, a tagged collection of (named) fields. */
// NOTE: to maximize interoperability, a struct is an object with at least
// a _scamperTag and kind field. The rest of the fields are the fields of the
// the struct. The order of arguments of a struct's constructor is the
// property order of the corresponding object, i.e., the order in which the
// fields are defined.
export interface Struct {
  [scamperTag]: 'struct',
  [structKind]: string,
  [key: string]: any,
  [key: number]: never
}

/** A syntax value wraps a normal value with source information. */
export type Syntax = {
  [scamperTag]: 'syntax',
  range: Range,
  value: Value
}

/** A raw object is an (opaque) Javascript value. */
export type Raw = Object

/** A tagged object is a value that is handled specially at runtime. */
export type TaggedObject = Closure | Char | Pair | Struct | Syntax

/** Values the main objects produced and manipulated during runtime. */
export type Value
  = boolean | number | string | Vector | undefined
  | TaggedObject | List | Function | symbol | Raw

/***** Value-querying Functions ***********************************************/

export function isBoolean (v: Value): boolean {
  return typeof v === 'boolean'
}

export function isNumber (v: Value): boolean {
  return typeof v === 'number'
}

export function isString (v: Value): boolean {
  return typeof v === 'string'
}

export function isArray (v: Value): boolean {
  return Array.isArray(v)
}

export function isVoid (v: Value): boolean {
  return v === undefined
}

export function isTaggedObject (v: Value): boolean {
  return v !== null && typeof v === 'object' && v.hasOwnProperty(scamperTag)
}

export function isClosure (v: Value): boolean {
  return isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'closure'
}

export function isChar (v: Value): boolean {
  return isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'char'
}

export function isPair (v: Value): boolean {
  return isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'pair'
}

export function isStruct (v: Value): boolean {
  return isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'struct'
}

export function isStructKind (v: Value, k: string): boolean {
  return isStruct(v) && (v as Struct)[structKind] === k
}

export function isList (v: Value): boolean {
  return v === null || (isPair(v) && (v as Pair).isList)
}

export function isJsFunction (v: Value): boolean {
  return typeof v === 'function'
}

export function isSyntax (v: Value): boolean {
  return isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'syntax'
}

export function isSymbol (v: Value): boolean {
  return typeof v === 'symbol'
}

/***** Value-manipulation Functions *******************************************/

export function listToVector (l: List): Value[] {
  const ret = []
  let cur = l
  while (cur !== null) {
    ret.push(cur.fst)
    cur = cur.snd as Pair
  }
  return ret
}

export function vectorToList (arr: Value[]): Pair | null {
  let ret = null
  for (let i = arr.length - 1; i >= 0; i--) {
    ret = mkPair(arr[i], ret)
  }
  return ret
}

export function mkClosure (
  arity: number, params: Id[], idx: OpIdx, env: Env, name?: Id): Closure {
  return { [scamperTag]: 'closure', arity, params, idx, env, name }
}

export function mkChar (c: string): Char {
  return { [scamperTag]: 'char', value: c }
}

export function mkPair (fst: Value, snd: Value): Pair {
  return {
    [scamperTag]: 'pair', fst, snd,
    isList: snd === null || isPair(snd) && (snd as Pair).isList
  }
}

export function mkList (...values: Value[]): List {
  return vectorToList(values)
}

export function mkStruct (
  kind: string, fields: string[], values: Value[]): Value {
  const ret: Struct = { [scamperTag]: 'struct', [structKind]: kind }
  for (let i = 0; i < fields.length; i++) {
    ret[fields[i]] = values[i]
  }
  return ret
}

export function mkSyntax (range: Range, value: Value): Syntax {
  return { [scamperTag]: 'syntax', range, value }
} 

export function mkSymbol (s: string): symbol {
  return Symbol.for(s)
}

export function getSymbolName (s: symbol): string {
  return Symbol.keyFor(s)! as string
}

export function getFieldsOfStruct (s: Struct): string[] {
  const ret = []
  for (const f in s) {
    ret.push(f)
  }
  return ret
}

export function stripSyntax (v: Value): Value {
  return isSyntax(v) ? (v as Syntax).value : v
}

export function stripAllSyntax (v: Value): Value {
  if (isSyntax(v)) {
    return stripAllSyntax((v as Syntax).value)
  } else if (isPair(v)) {
    const p = v as Pair
    return mkPair(stripAllSyntax(p.fst), stripAllSyntax(p.snd))
  } else if (isArray(v)) {
    return (v as Value[]).map(stripAllSyntax)
  } else if (isStruct(v)) {
    const s = v as Struct
    const fields = getFieldsOfStruct(s)
    return mkStruct(s[structKind], fields, fields.map((f) => stripAllSyntax(s[f])))
  } else {
    return v
  }
}

export function unpackSyntax (v: Value): { range: Range, value: Value } {
  return isSyntax(v) ?
    { range: (v as Syntax).range, value: (v as Syntax).value } :
    { range: noRange, value: v }
}

export function rangeOf (v: Value): Range {
  return isSyntax(v) ? (v as Syntax).range : noRange
}

export const nameFn = (name: string, fn: Function): Function =>
  Object.defineProperty(fn, 'name', { value: name })

/***** Virtual Machine Types **************************************************/

export type PVar = { tag: 'var', id: Id }
export type PValue = { tag: 'value', value: Value }
export type PCtor = { tag: 'ctor', id: Id, args: Pattern[] }

export type Pattern = PVar | PValue | PCtor
export type MatchAlt = { pattern: Pattern, pc: OpIdx }
export type PatternMatch = MatchAlt[]

/** The object map stores constant values within a compiled program. */
export type ObjectMap = Value[]

/** The pattern map stores pattern match branches. */
export type PatternMap = PatternMatch[]

/** The identifier map stores names of program identifiers. */
export type IdentifierMap = string[]

/**
 * A compiled program bundles together opcodes with the various support maps.
 */
export type CompiledProgram = {
  objects: ObjectMap,
  patterns: PatternMap,
  identifiers: IdentifierMap,
  ops: OpCodes
}