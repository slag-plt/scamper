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
  name?: Id,
  // N.B., call is required so that Javascript code can call Scamper closures similarly
  // to Javascript functions. Since closures are generated during runtime, the underlying
  // Machine can be referenced by call to perform evaluation.
  call: (...args: any) => any
}

/** A char is a tagged object that captures a single character (a one-character string). */
export interface Char extends TaggedObject {
  [scamperTag]: 'char',
  value: string
}

/** A symbol is tagged object that contains a string interned for fast equality. */
export interface Sym extends TaggedObject {
  [scamperTag]: 'sym', value: string
} 

/** A pair is a tagged object that contains a pair of (potentially heterogeneous) values. */
export interface Pair extends TaggedObject {
  [scamperTag]: 'pair',
  fst: Value,
  snd: Value,
  isList: boolean
}

/**
 * A pattern variable appears in pattern objects and denotes a binding
 * location in the pattern. Note that a negative index indicates a
 * wildcard (non-binding) variable.
 */
export interface PVar extends TaggedObject { [scamperTag]: 'pvar', idx: number }

// NOTE: to maximize interoperability, a struct is an object with at least
// a _scamperTag and kind field. The rest of the fields are the fields of the
// the struct.
// 
// An invariant of a Scamper object is that the order of arguments of a struct's
// constructor is the property order of the corresponding object, i.e., the
// order in which the fields are defined.
export interface Struct extends TaggedObject {
  [scamperTag]: 'struct',
  [structKind]: string,
  [key: string]: any,
  [key: number]: never
}

/**
 * A Scamper list is either null or a pair whose second component is a list,
 * recursively.
 */
export type List = null | Pair

/** A Scamper vector is a Javascript array of values. */
export type Vector = Value[]

/** A Scamper function is either a closure or a raw Javascript function. */
export type ScamperFn = Closure | Function

/** Raw Javascript values are any Javascript object. */
export type Raw = Object

/** Values are the core datatype manipulated by Scamper programs. */
export type Value = boolean | number | string | List | Vector | Function | undefined | TaggedObject | Raw