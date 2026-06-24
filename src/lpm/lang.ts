import { Range } from "./range.js"
import { FunctionDoc } from "../scheme/docstring/docstring"

///// Runtime values ///////////////////////////////////////////////////////////

/** The field name of Scamper objects denoting that object's runtime tag. */
export const scamperTag = "##scamperTag##"

/** The field name of Scamper objects that are structs denoting that struct's kind. */
export const structKind = "##structKind##"

/** Identifiers name entities maintained at runtime. */
export type Id = string

/** Indices provide "fast names" of objects, in particular locals, at runtime. */
export type Idx = number

/**
 * Environments are collections of variable bindings. The overall runtime
 * environment captures three different scopes:
 * 
 * + `imports`: the collection of imported module names
 * + `topLevel`: the collection of top-level (module-level) bindings
 * + `locals`: the collection of local bindings
 * 
 * When resolving a (simple) variable name, we search in order of increasing
 * scope: local, top-level, and then imports.
 * 
 * Environments are also _immutable_: operations return new environments rather
 * mutating the current environment.
 */
export class Env {
  /** A mapping of imported modules to their bound libraries */
  private imports: Map<string, Library>
  /** A mapping of top-level (module-level) bindings */
  private topLevel: Map<string, Value>
  /** A mapping of local bindings */
  private locals: Map<string, Value>

  /** Constructs a new environemnt from the given maps */
  constructor(imports: Map<string, Library>, topLevel: Map<string, Value>, locals: Map<string, Value>) {
    this.imports = imports
    this.topLevel = topLevel
    this.locals = locals
  }

  /** The empty environment */
  static empty: Env = new Env(new Map(), new Map(), new Map())

  /**
   * @param name the (simple) name of the variable to look up
   * @return the value bound to this variable name or undefined if it does not
   *         exist
   */
  get(name: string): Value | undefined {
    const localValue = this.locals.get(name)
    if (localValue !== undefined) {
      return localValue
    }
    const topLevelValue = this.topLevel.get(name)
    if (topLevelValue !== undefined) {
      return topLevelValue
    }
    for (const library of this.imports.values()) {
      if (library.bindings.has(name)) {
        return library.bindings.get(name)
      }
    }
    return undefined
  }

  /**
   * @param name the (simple) name of the variable to look up
   * @return true iff the variable is bound in this environment
   */
  has(name: string): boolean {
    return this.locals.has(name) ||
      this.topLevel.has(name) ||
      [...this.imports.values()].some(lib =>
        lib.bindings.has(name))
  }

  extendWithImport(name: string, lib: Library): Env {
    return new Env(
      new Map([...this.imports, [name, lib]]),
      this.topLevel,
      this.locals
    )
  }

  extendWithTopLevel(name: string, value: Value): Env {
    return new Env(
      this.imports,
      new Map([...this.topLevel, [name, value]]),
      this.locals
    )
  }

  extendWithLocals(...locals: [string, Value][]): Env {
    return new Env(
      this.imports,
      this.topLevel,
      new Map([...this.locals, ...locals])
    )
  }

  extendReplacingLocals(...locals: [string, Value][]): Env {
    return new Env(
      this.imports,
      this.topLevel,
      new Map(locals)
    )
  }

  withoutLocals(...names: string[]): Env {
    return new Env(
      this.imports,
      this.topLevel,
      new Map([...this.locals].filter(([x, _v]) => !(x in names)))
    )
  }
}

type InitializerFunction = () => Promise<void>
/** A library is a collection of importable top-level definitions. */
export class Library {
  bindings: Map<string, Value>
  initializer?: InitializerFunction

  constructor(initializer?: InitializerFunction) {
    this.bindings = new Map()
    this.initializer = initializer
  }

  registerValue(name: string, v: Value) {
    if (typeof v === "function") {
      Object.defineProperty(v, "name", { value: name })
    }
    this.bindings.set(name, v)
  }

  static fromLibs(...libs: Library[]): Library {
    const initializer = async () => {
      for (const lib of libs) {
        if (lib.initializer !== undefined) {
          await lib.initializer()
        }
      }
    }
    const ret = new Library(initializer)
    for (const lib of libs) {
      for (const [name, value] of lib.bindings) {
        ret.registerValue(name, value)
      }
    }
    return ret
  }
}

/** Tagged objects are Scamper values with a queryable runtime identity. */
export interface TaggedObject {
  [scamperTag]: string
}

/** A closure is a tagged object that bundles a function with its captured environment. */
export interface Closure extends TaggedObject {
  [scamperTag]: "closure"
  params: Id[]
  code: Blk
  env: Env
  // N.B., call is required so that Javascript code can call Scamper closures similarly
  // to Javascript functions. Since closures are generated during runtime, the underlying
  // Machine can be referenced by call to perform evaluation.
  call: (...args: Value[]) => Value
  name?: Id
}

/** A char is a tagged object that captures a single character (a one-character string). */
export interface Char extends TaggedObject {
  [scamperTag]: "char"
  value: string
}

/** A symbol is a tagged object representing an identifier. */
export interface Sym extends TaggedObject {
  [scamperTag]: "sym"
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
  [scamperTag]: "struct"
  [structKind]: string
  [key: string]: Value
  [key: number]: never
}

/** A Scamper vector is a Javascript array of values. */
export type Vector = Value[]

/** A Scamper function is either a closure or a raw Javascript function. */
export type JsFunction = (...args: Value[]) => Value
export type ScamperFn = Closure | JsFunction

/** Calls a ScamperFn function with the provided arguments */
export function callScamperFn(fn: ScamperFn, ...args: Value[]): any {
  if (typeof fn === "function") {
    return fn(...args)
  } else {
    return fn.call(...args)
  }
}

/** Raw Javascript values are any Javascript object. */
export type Raw = object

/** Values are the core datatype manipulated by LPM programs. */
export type Value =
  | number
  | boolean
  | string
  | null
  | undefined
  | Vector
  | TaggedObject
  | ScamperFn
  | Raw

// N.B., We follow Clojure's lead and distinguish between pairs and lists
// explicitly. While they are defined as algebraic datatypes, pairs and lists
// are common enough that are "built-in" datatypes to the runtime.

/**
 * A pair is an algebraic datatype with a first and second component.
 */
export interface Pair extends Struct {
  [scamperTag]: "struct"
  [structKind]: "pair"
  fst: Value
  snd: Value
}

/**
 * A (non-empty) cons cell is an algebraic datatype representing a non-empty list
 * with a head and tail. The tail, itself, must be a list.
 */
export interface Cons extends Struct {
  [scamperTag]: "struct"
  [structKind]: "cons"
  head: Value
  tail: List
}

/** A list is either empty (null) or non-empty (cons) */
export type List = null | Cons

///// The Little Pattern Machine language //////////////////////////////////////

export interface Lit {
  tag: "lit"
  value: Value
  range: Range
}
export interface Var {
  tag: "var"
  name: string
  range: Range
}
export interface Ctor {
  tag: "ctor"
  name: string
  fields: string[]
  range: Range
}
export interface Cls {
  tag: "cls"
  params: string[]
  body: Blk
  name?: string
  range: Range
}
export interface Ap {
  tag: "ap"
  numArgs: number
  range: Range
}
export interface Match {
  tag: "match"
  branches: [Pat, Blk][]
  range: Range
  // hack fix to not modify original branch
  // TODO: making this better requires better bytecode
  currBranchIdx?: number
}
export interface Raise {
  tag: "raise"
  msg: string
  range: Range
}
export interface PopS {
  tag: "pops"
}
export interface PopV {
  tag: "popv"
}
export interface Rept {
  tag: "rept"
  range: Range
}
export type Ops =
  | Lit
  | Var
  | Ctor
  | Cls
  | Ap
  | Match
  | Raise
  | PopS
  | PopV
  | Rept
export type Blk = Ops[]

export interface Disp {
  tag: "disp"
  expr: Blk
  range: Range
}
export interface Import {
  tag: "import"
  name: string
  range: Range
}
export interface Define {
  tag: "define"
  name: string
  expr: Blk
  range: Range
  doc?: FunctionDoc
}
export interface StmtExp {
  tag: "stmtexp"
  expr: Blk
  range: Range
}
export type Stmt = Disp | Import | Define | StmtExp
export type Prog = Stmt[]

export interface PWild {
  tag: "pwild"
  range: Range
}
export interface PLit {
  tag: "plit"
  value: Value
  range: Range
}
export interface PVar {
  tag: "pvar"
  name: string
  range: Range
}
export interface PCtor {
  tag: "pctor"
  name: string
  args: Pat[]
  range: Range
}
export type Pat = PWild | PLit | PVar | PCtor
