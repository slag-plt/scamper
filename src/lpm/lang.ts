import { Range } from './range.js'

///// Runtime values ///////////////////////////////////////////////////////////

/** The field name of Scamper objects denoting that object's runtime tag. */
export const scamperTag = '##scamperTag##'

/** The field name of Scamper objects that are structs denoting that struct's kind. */
export const structKind = '##structKind##'

/** Identifiers name entities maintained at runtime. */
export type Id = string

/** Indices provide "fast names" of objects, in particular locals, at runtime. */
export type Idx = number

/** Environments are scoped collections of variable bindings. */
export class Env {
  bindings: Map<string, Value>
  parent?: Env

  constructor (parent?: Env) {
    this.bindings = new Map()
    this.parent = parent
  }

  get (name: string): Value | undefined {
    return this.bindings.has(name) ?
      this.bindings.get(name) : this.parent?.get(name)
  }

  set (name: string, value: Value): void {
    this.bindings.set(name, value)
  }

  has (name: string): boolean {
    return this.bindings.has(name) || 
      ((this.parent === undefined) ? false : this.parent.has(name))
  }

  extend (...bindings: [string, Value][]): Env {
    const ret = new Env(this)
    for (const [name, value] of bindings) {
      ret.set(name, value)
    }
    return ret
  }

  pop (): Env {
    return this.parent === undefined ? new Env() : this.parent
  }
}

/** A library is a collection of importable top-level definitions. */
export class Library {
  lib: [string, Value][]
  initializer: Function | undefined

  constructor (initializer?: Function) {
    this.lib = []
    this.initializer = initializer
  }

  registerValue (name: string, v: Value) {
    if (typeof v === 'function') {
      Object.defineProperty(v, 'name', { value: name })
    }
    this.lib.push([name, v])
  }
}

/** Tagged objects are Scamper values with a queryable runtime identity. */
export interface TaggedObject {
  [scamperTag]: string
}

/** A closure is a tagged object that bundles a function with its captured environment. */
export interface Closure extends TaggedObject {
  [scamperTag]: 'closure',
  params: Id[],
  code: Blk,
  env: Env,
  // N.B., call is required so that Javascript code can call Scamper closures similarly
  // to Javascript functions. Since closures are generated during runtime, the underlying
  // Machine can be referenced by call to perform evaluation.
  call: (...args: Value[]) => Value
  name?: Id,
}

/** A char is a tagged object that captures a single character (a one-character string). */
export interface Char extends TaggedObject {
  [scamperTag]: 'char',
  value: string
}

/** A symbol is a tagged object representing an identifier. */
export interface Sym extends TaggedObject {
  [scamperTag]: 'sym',
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

/** Calls a ScamperFn function with the provided arguments */
export function callScamperFn (fn: ScamperFn, ...args: Value[]): any {
  if (typeof fn === 'function') {
    return fn(...args)
  } else {
    return fn.call(...args)
  }
}

/** Raw Javascript values are any Javascript object. */
export type Raw = object

/** Values are the core datatype manipulated by LPM programs. */
export type Value =
  number | boolean | string | null | undefined |
  Vector | TaggedObject | ScamperFn | Raw

// N.B., We follow Clojure's lead and distinguish between pairs and lists
// explicitly. While they are defined as algebraic datatypes, pairs and lists
// are common enough that are "built-in" datatypes to the runtime.

/**
 * A pair is an algebraic datatype with a first and second component.
 */
export interface Pair extends Struct {
  [scamperTag]: 'struct',
  [structKind]: 'pair',
  fst: Value,
  snd: Value
}

/**
 * A (non-empty) cons cell is an algebraic datatype representing a non-empty list
 * with a head and tail. The tail, itself, must be a list.
 */
export interface Cons extends Struct {
  [scamperTag]: 'struct',
  [structKind]: 'cons',
  head: Value,
  tail: List
}

/** A list is either empty (null) or non-empty (cons) */
export type List = null | Cons

///// The Little Pattern Machine language //////////////////////////////////////

export type Lit    = { tag: 'lit', value: Value, range: Range }
export type Var    = { tag: 'var', name: string, range: Range }
export type Ctor   = { tag: 'ctor', name: string, fields: string[], range: Range }
export type Cls    = { tag: 'cls', params: string[], body: Blk, name?: string, range: Range }
export type Ap     = { tag: 'ap', numArgs: number, range: Range }
export type Match  = { tag: 'match', branches: [Pat, Blk][], range: Range }
export type Raise  = { tag: 'raise', msg: string, range: Range }
export type PopS   = { tag: 'pops' }
export type PopV   = { tag: 'popv' }
export type Ops    = Lit | Var | Ctor | Cls | Ap | Match | Raise | PopS | PopV
export type Blk    = Ops[]

export type Disp    = { tag: 'disp', expr: Blk, range: Range }
export type Import  = { tag: 'import', name: string, range: Range }
export type Define  = { tag: 'define', name: string, expr: Blk, range: Range }
export type StmtExp = { tag: 'stmtexp', expr: Blk, range: Range }
export type Stmt    = Disp | Import | Define | StmtExp
export type Prog    = Stmt[]

export type PWild  = { tag: 'pwild', range: Range }
export type PLit   = { tag: 'plit', value: Value, range: Range }
export type PVar   = { tag: 'pvar', name: string, range: Range }
export type PCtor  = { tag: 'pctor', name: string, args: Pat[], range: Range }
export type Pat    = PWild | PLit | PVar | PCtor

/**
 * A stack frame records all relevant to track the execution of a single function call.
 */
export class Frame {
  name: string
  env: Env
  values: Value[]
  ops: Ops[]

  constructor (name: string, env: Env, blk: Blk) {
    this.name = name
    this.env = env
    this.values = []
    this.ops = blk.toReversed()
  }

  isFinished (): boolean {
    return this.ops.length === 0
  }

  pushBlk (blk: Blk) {
    this.ops.push(...blk.toReversed())
  }

  popInstr (): Ops {
    return this.ops.pop()!
  }
}

/** A single thread of execution in LPM. */
export class Thread {
  name: string
  prog: Prog
  curStmt: number
  env: Env
  frames: Frame[]
  results: Value[]

  constructor (name: string, env: Env, prog: Prog) { 
    this.name = name
    this.prog = prog
    this.curStmt = 0
    this.env = env
    this.frames = []
    this.results = []
    this.setupNextStmt()
  }

  setupNextStmt (): void {
    const stmt = this.getCurrentStmt()
    switch (stmt.tag) {
      case 'disp': {
        this.push(`##stmt_{thread.curStmt}##`, this.env, stmt.expr)
        break
      }
      case 'import': {
        // N.B., no frame setup required
        break
      }
      case 'define': {
        this.push(`##stmt_{thread.curStmt}##`, this.env, stmt.expr)
        break
      }
      case 'stmtexp': {
        this.push(`##stmt_{thread.curStmt}##`, this.env, stmt.expr)
        break
      }
    }
  }

  advanceStmt (): void {
    this.frames = []
    this.curStmt++
    if (!this.isFinished()) { this.setupNextStmt() }
  }

  getCurrentStmt (): Stmt {
    return this.prog[this.curStmt]
  }

  isFinished (): boolean {
    return this.curStmt >= this.prog.length
  }

  getCurrentFrame (): Frame {
    return this.frames[this.frames.length - 1]
  }

  push (name: string, env: Env, blk: Blk): void {
    this.frames.push(new Frame(name, env, blk))
  }

  pop (): void {
    this.frames.pop()
  }

  unwindToNextStatement (): void {
    this.frames = []
    this.results.push(undefined)
    this.advanceStmt()
  }
}