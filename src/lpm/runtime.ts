/** Locations are used to track single positions within source code. */
export class Loc {
  line: number
  col: number
  idx: number

  constructor (line: number, col: number, idx: number) {
    this.line = line
    this.col = col
    this.idx = idx
  }

  public toString (): string {
    return `${this.line}:${this.col}`
  }

  static none: Loc = new Loc(-1, -1, -1)
}

/** Ranges bundle start and end locations within source code. */
export class Range {
  begin: Loc
  end: Loc

  constructor (begin: Loc, end: Loc) {
    this.begin = begin
    this.end = end
  }
 
  public toString (): string {
    return `${this.begin.toString()}-${this.end.toString()}`
  }

  static none: Range = new Range(Loc.none, Loc.none)
}

/** Phases of scamper execution, used for the purposes of error reporting. */
type Phase = 'Parser' | 'Runtime'

/** Errors that arise during Scamper compilation and execution. */
export class ScamperError extends Error {
  phase: Phase
  modName?: string
  range?: Range
  source?: string

  constructor (phase: Phase, msg: string, modName?: string, range?: Range, source?: string) {
    super(msg)
    this.phase = phase
    this.modName = modName
    this.range = range
    this.source = source
  }

  toString(): string {
    const detail = `${this.modName ? this.modName : ''}${(this.range && this.range !== Range.none) ? this.range.toString() : ''}`
    const src = this.source ? `(${this.source}) ` : ''
    return `${this.phase} error${detail.length > 0 ? ' [' + detail + ']' : ''}: ${src}${this.message}`
  }
}

/** Internal compiler errors arise due to bugs in Scamper. */
export class ICE extends Error {
  funcName: string

  constructor (funcName: string, msg: string) {
    super(msg)
    this.funcName = funcName
  }

  toString(): string {
    return `ICE (${this.funcName}): ${this.message}\n${this.stack}`
  }
}

/** Identifiers name entities maintained at runtime. */
export type Id = string

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

/** Libraries are collections of (top-level) bindings from names to values. */
export class Library {
  lib: [string, Value][]
  initializer: Function | undefined

  constructor () {
    this.lib = []
    this.initializer = undefined
  }

  registerValue (name: string, v: Value) {
    if (typeof v === 'function') {
      nameFn(name, v)
    }
    this.lib.push([name, v])
  }
}

/**
 * Local maps record local variable indices to variable names for the
 * purposes of debugging and error reporting.
 */
type LocalMap = string[]

/** The field name of Scamper objects denoting that object's runtime tag. */
export const scamperTag = Symbol('tag')

/** The field name of Scamper objects that are structs denoting that struct's kind. */
export const structKind = Symbol('kind')

/** A closure is a tagged object that bundles a function with its captured environment. */
export type Closure = {
  [scamperTag]: 'closure',
  params: Id[],
  code: Id,
  env: Env,
  name?: string
  localMap?: LocalMap,
}

/** A char is a tagged object that captures a single character (a one-character string). */
export type Char = { [scamperTag]: 'char', value: string }

/** A symbol is tagged object that contains a string interned for fast equality. */
export type Sym  = { [scamperTag]: 'sym', value: string } 

/** A pair is a tagged object that contains a pair of (potentially heterogeneous) values. */
export type Pair = { [scamperTag]: 'pair', fst: Value, snd: Value, isList: boolean }

/**
 * A pattern variable appears in pattern objects and denotes a binding
 * location in the pattern. Note that a negative index indicates a
 * wildcard (non-binding) variable.
 */
export type PVar = { [scamperTag]: 'pvar', idx: number }

/** A syntax object is a tagged object that wraps a Scamper object with source code information. */
export type Syntax = { [scamperTag]: 'syntax', range: Range, value: Value }

/** Tagged objects are Scamper values with a queryable runtime identity. */
export type TaggedObject = Closure | Char | Sym | Pair | PVar | Syntax | Struct

// NOTE: to maximize interoperability, a struct is an object with at least
// a _scamperTag and kind field. The rest of the fields are the fields of the
// the struct.
// 
// An invariant of a Scamper object is that the order of arguments of a struct's
// constructor is the property order of the corresponding object, i.e., the
// order in which the fields are defined.
export interface Struct {
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

///// Predicates for determining the type of a Scamper value.

export const isNumber = (v: Value): boolean => typeof v === 'number'
export const isBoolean = (v: Value): boolean => typeof v === 'boolean'
export const isString = (v: Value): boolean => typeof v === 'string'
export const isNull = (v: Value): boolean => v === null
export const isVoid = (v: Value): boolean => v === undefined
export const isArray = (v: Value): boolean => Array.isArray(v)
export const isJsFunction = (v: Value): boolean => typeof v === 'function'

export const isTaggedObject = (v: Value): boolean =>
  v !== null && typeof v === 'object' && v.hasOwnProperty(scamperTag)
export const isClosure = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'closure'
export const isFunction = (v: Value): boolean => isJsFunction(v) || isClosure(v)
export const isChar = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'char'
export const isSym = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'sym'
export const isSymName = (v: Value, name: string): boolean => isSym(v) && (v as Sym).value === name
export const isPair = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'pair'
export const isList = (v: Value): boolean => v === null || (isPair(v) && (v as Pair).isList)
export const isPVar = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'pvar'
export const isSyntax = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'syntax'
export const isStruct = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'struct'
export const isStructKind = (v: Value, k: string): boolean => isStruct(v) && (v as Struct)[structKind] === k

///// Constructor functions for making Scamper values.

export const mkClosure = (arity: number, params: Id[], code: Id, env: Env): Value =>
  ({ [scamperTag]: 'closure', arity, params, code, env })
export const mkChar = (v: string): Char => ({ [scamperTag]: 'char', value: v })
export const mkSym = (v: string): Sym => ({ [scamperTag]: 'sym', value: v })
export const mkPair = (fst: Value, snd: Value): Pair => ({
  [scamperTag]: 'pair', fst, snd,
  isList: snd === null || ((isPair(snd) && (snd as Pair).isList))
})
export const mkList = (...values: Value[]): List => vectorToList(values)
export const mkPVar = (idx: number): PVar => ({ [scamperTag]: 'pvar', idx })
export const mkSyntax = (range: Range, value: Value): Syntax =>
  ({ [scamperTag]: 'syntax', range, value })
export const mkStruct = (kind: string, fields: string[], values: Value[]): Value => {
  const ret: Struct = { [scamperTag]: 'struct', [structKind]: kind }
  for (let i = 0; i < fields.length; i++) {
    ret[fields[i]] = values[i]
  }
  return ret
}

///// Utility functions for manipulating Scamper values.

/** @return a branch object suitable for pattern matching */
export function mkBranch (pattern: Value, target: number): Pair {
  return mkPair(pattern, target)
}

/** @return v but with its top-level syntax wrapper removed, if it exists. */
export const stripSyntax = (v: Value): Value =>
  isSyntax(v) ? (v as Syntax).value : v

/** @return v but with all syntax wrappers removed, recursively. */
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

/** @returns a pair of a syntax object and its wrapped value. */
export const unpackSyntax = (v: Value): { range: Range, value: Value } =>
  isSyntax(v) ?
    { range: (v as Syntax).range, value: (v as Syntax).value } :
    { range: Range.none, value: v }

/** @returns the range component of a syntax object. */
export const rangeOf = (v: Value): Range =>
  isSyntax(v) ? (v as Syntax).range : Range.none

/** Mutates a Javascript function to contain a `name` field with that function's name. */
export const nameFn = (name: string, fn: Function): Function =>
  Object.defineProperty(fn, 'name', { value: name })

/** @return a vector (array) representation of the input list. */
export function listToVector (l: List): Value[] {
  const ret: Value[] = []
  let cur = l
  while (cur !== null) {
    ret.push(cur.fst)
    cur = cur.snd as Pair
  }
  return ret
}

/** @return a list representation of the input vector (array). */
export function vectorToList (arr: Value[]): Pair | null {
  let ret: Pair | null = null
  for (let i = arr.length - 1; i >= 0; i--) {
    ret = mkPair(arr[i], ret)
  }
  return ret
}

/** @return a list of the fields of the given struct. */
export function getFieldsOfStruct (s: Struct): string[] {
  const ret: string[] = []
  for (const f in s) {
    ret.push(f)
  }
  return ret
}

/**
 * @returns the type of the given value as a string (for debugging purposes).
 */
export function typeOf (v: Value): string {
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
    return `[Function: ${(v as Closure).name}]`
  } else if (isChar(v)) {
    return `char`
  } else if (isSym(v)) {
    return `symbol`
  } else if (isPair(v)) {
    return `pair`
  } else if (isList(v)) {
    return `list`
  } else if (isSyntax(v)) {
    return `[Syntax: ${typeOf((v as Syntax).value)}]`
  } else if (isStruct(v)) {
    return `[Struct: ${(v as Struct)[structKind]}]`
  } else {
    return typeof v
  }
}

/** The code of a LPM program is an array of bytes. */
export type Code = {
  ops: Uint8Array,
  numLocals: number,
}

/**
 * A program collects together three structures:
 * + `code`: a mapping from function identifiers to their code.
 * + `identifiers`: a mapping from string identifiers to their strings.
 * + `objects`: a mapping from object identifiers to their values.
 */
export type Program = {
  code: Map<Id, Code>
  identifiers: string[],
  objects: Value[],
}

export function mkProgram (): Program {
  return {
    code: new Map<Id, Code>(),
    identifiers: [],
    objects: []
  }
}