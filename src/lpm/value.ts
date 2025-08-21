/** The field name of Scamper objects denoting that object's runtime tag. */
export const scamperTag = '##scamperTag##'

/** The field name of Scamper objects that are structs denoting that struct's kind. */
export const structKind = '##structKind##'

/** Identifiers name entities maintained at runtime. */
export type Id = string

/** Indices provide "fast names" of objects, in particular locals, at runtime. */
export type Idx = number

/**
 * Environments capture (scoped) mappings from identifiers to values. We
 * ensure that the length of the environment reflects the (statically known)
 * number of local variables in the current function scope with the following
 * layout:
 * 
 * ~~~
 * [<captured scope>, <params>, <locals>] 
 * ~~~
*/
export type Env = Value[]

/** Tagged objects are Scamper values with a queryable runtime identity. */
interface TaggedObject {
  [scamperTag]: string
}

/** A closure is a tagged object that bundles a function with its captured environment. */
export interface Closure extends TaggedObject {
  [scamperTag]: 'closure',
  params: Idx[],
  code: Idx,
  env: Env,
  // N.B., call is required so that Javascript code can call Scamper closures similarly
  // to Javascript functions. Since closures are generated during runtime, the underlying
  // Machine can be referenced by call to perform evaluation.
  call: (...args: any) => any
  name?: Id,
}

/** A char is a tagged object that captures a single character (a one-character string). */
export interface Char extends TaggedObject {
  [scamperTag]: 'char',
  value: string
}

// NOTE: to maximize interoperability, a struct is an object with at least
// a ##scamperTag## and ##kind## field. The rest of the fields are the fields of the
// the struct.
// 
// An invariant of a Scamper struct is that the order of arguments of a struct's
// constructor is the property order of the corresponding object, i.e., the
// order in which the fields are defined.
//
// Additionally, fields denoted with ##...## are considered _internal_ fields that
// are not part of the struct's arguments.
export interface Struct extends TaggedObject {
  [scamperTag]: 'struct',
  [structKind]: string,
  [key: string]: any,
  [key: number]: never
}

/** A Scamper vector is a Javascript array of values. */
export type Vector = Value[]

/** A Scamper function is either a closure or a raw Javascript function. */
export type ScamperFn = Closure | Function

/** Raw Javascript values are any Javascript object. */
export type Raw = object

/** Values are the core datatype manipulated by Scamper programs. */
export type Value = number | boolean | string | symbol | null | undefined | Vector | TaggedObject | ScamperFn | Raw

///// Value predicates /////////////////////////////////////////////////////////

export const isNumber = (v: Value): v is number => typeof v === 'number'
export const isBoolean = (v: Value): v is boolean => typeof v === 'boolean'
export const isString = (v: Value): v is string => typeof v === 'string'
export const isSym = (v: Value): v is symbol => typeof v == 'symbol'
export const isSymName = (v: Value, name: string): boolean => isSym(v) && Symbol.keyFor(v) === name
export const isNull = (v: Value): v is null => v === null
export const isVoid = (v: Value): v is undefined => v === undefined
export const isArray = (v: Value): v is Array<Value> => Array.isArray(v)
export const isTaggedObject = (v: Value): v is TaggedObject =>
  v !== null && typeof v === 'object' && v.hasOwnProperty(scamperTag)
export const isJsFunction = (v: Value): v is Function => typeof v === 'function'
export const isClosure = (v: Value): v is Closure => isTaggedObject(v) && v[scamperTag] === 'closure'
export const isFunction = (v: Value): v is ScamperFn => isJsFunction(v) || isClosure(v)
export const isChar = (v: Value): v is Char => isTaggedObject(v) && v[scamperTag] === 'char'
export const isStruct = (v: Value): v is Struct => isTaggedObject(v) && v[scamperTag] === 'struct'
export const isStructKind = <T extends Struct> (v: Value, k: string): v is T => isStruct(v) && v[structKind] === k

///// Constructors /////////////////////////////////////////////////////////////

export const mkClosure = (params: Idx[], code: Idx, env: Env, call: (...args: any) => any, name?: Id): Closure =>
  ({ [scamperTag]: 'closure', params, code, env, call, name })
export const mkChar = (v: string): Char => ({ [scamperTag]: 'char', value: v })
export const mkSym = (v: string): symbol => Symbol(v)
export const mkStruct = (kind: string, fields: string[], values: Value[]): Struct => {
  const ret: Struct = { [scamperTag]: 'struct', [structKind]: kind }
  for (let i = 0; i < fields.length; i++) {
    ret[fields[i]] = values[i]
  }
  return ret
}

///// Lists ////////////////////////////////////////////////////////////////////

// TODO: Clojure distinguishes between pairs and lists! In particular, the second
// argument of cons must be a list. Do we care to make that distinction in
// Scamper, especially its core?

/**
 * A pair is an algebraic datatype with a first and second component.
 */
interface Pair extends Struct {
    [scamperTag]: 'struct',
    [structKind]: 'pair',
    fst: Value,
    snd: Value,
    '##isList##': boolean
}

/**
 * A list is either null or a pair whose second component is a list, recursively.
 */
type List = null | Pair