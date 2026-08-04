import { Range } from './range.js'
import { ICE, ScamperError } from './error.js'

///// Runtime values ///////////////////////////////////////////////////////////

/** The field name of Scamper objects denoting that object's runtime tag. */
export const scamperTag = '##scamperTag##'

/** The field name of Scamper objects that are structs denoting that struct's kind. */
export const structKind = '##structKind##'

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
/**
 * Sentinel for a local binding that has been declared (its name is in scope)
 * but not yet assigned -- the transient state of a `let` binding whose value is
 * still being evaluated. Looking one up is a "referenced before defined"
 * runtime error; a thunk that captured the scope sees the value once filled.
 */
export const HOLE: unique symbol = Symbol('hole')

/** A local binding scope: names to values (or HOLE while still unassigned). */
export type Scope = Map<string, Value | typeof HOLE>

export class Env {
  /** A mapping of imported modules to their bound libraries */
  private imports: Map<string, Module>
  /** A mapping of top-level (module-level) bindings */
  private topLevel: Map<string, Value>
  /** A stack of local binding scopes; the last element is the innermost. */
  private locals: Scope[]

  /** Constructs a new environemnt from the given maps */
  constructor(
    imports: Map<string, Module>,
    topLevel: Map<string, Value>,
    locals: Scope[],
  ) {
    this.imports = imports
    this.topLevel = topLevel
    this.locals = locals
  }

  /** The empty environment */
  static empty: Env = new Env(new Map(), new Map(), [])

  /**
   * @param name the (simple) name of the variable to look up
   * @return the value bound to this variable name or undefined if it does not
   *         exist
   */
  /**
   * Resolve a name without throwing: a found slot (which may be a HOLE for an
   * as-yet-unassigned local), or not-found. Used by get() and by the raiser,
   * which must tolerate holes.
   */
  lookup(
    name: string,
  ): { found: true; slot: Value | typeof HOLE } | { found: false } {
    // 1. Local scopes, innermost (most recently pushed) first
    for (let i = this.locals.length - 1; i >= 0; i--) {
      const scope = this.locals[i]
      if (scope.has(name)) {
        return { found: true, slot: scope.get(name) }
      }
    }
    // 2. Top-level scope
    if (this.topLevel.has(name)) {
      return { found: true, slot: this.topLevel.get(name) }
    }
    // 3. Imported modules, most recent imports first
    for (const library of [...this.imports.values()].toReversed()) {
      if (library.bindings.has(name)) {
        return { found: true, slot: library.bindings.get(name) }
      }
    }
    return { found: false }
  }

  /**
   * @param name the (simple) name of the variable to look up
   * @return the value bound to this variable name
   */
  get(name: string): Value {
    const r = this.lookup(name)
    if (!r.found) {
      throw new ScamperError(
        'Runtime',
        `Attempted to look up variable "${name}" but it is not bound in this environment!`,
      )
    }
    if (r.slot === HOLE) {
      throw new ScamperError(
        'Runtime',
        `Variable "${name}" is referenced before it is defined`,
      )
    }
    return r.slot
  }

  /** @return the top-level bindings of this environment as a Module */
  getTopLevelAsModule(): Module {
    const ret = new Module()
    for (const [name, value] of this.topLevel) {
      ret.registerValue(name, value)
    }
    return ret
  }

  /**
   * @return all in-scope local bindings flattened into one map (innermost
   *         scope wins). Used to snapshot a closure's captured environment.
   */
  getLocals(): Map<string, Value> {
    const flat = new Map<string, Value>()
    for (const scope of this.locals) {
      for (const [name, slot] of scope) {
        if (slot !== HOLE) flat.set(name, slot)
      }
    }
    return flat
  }

  /**
   * @return the local scope stack by reference (a shallow copy of the array;
   *         the scope objects are shared). Closures capture this so that a
   *         binding filled after the closure is created -- letrec -- is visible.
   */
  getScopes(): Scope[] {
    return [...this.locals]
  }

  /**
   * Push a fresh innermost scope declaring each of `names` as a HOLE (in scope
   * but unassigned). `assign` later fills the holes. Used by `let`.
   */
  declareScope(names: string[]): Env {
    const scope: Scope = new Map(names.map((n) => [n, HOLE]))
    return new Env(this.imports, this.topLevel, [...this.locals, scope])
  }

  /**
   * Fill a declared binding in place, mutating the scope object so that any
   * closure that captured it sees the value. Assigns to the innermost scope
   * that declares `name`.
   */
  assign(name: string, value: Value): void {
    for (let i = this.locals.length - 1; i >= 0; i--) {
      if (this.locals[i].has(name)) {
        this.locals[i].set(name, value)
        return
      }
    }
    throw new ICE('Env.assign', `assigned undeclared local "${name}"`)
  }

  /** Replace the local scope stack wholesale (used to build a callee frame). */
  withLocalScopes(scopes: Scope[]): Env {
    return new Env(this.imports, this.topLevel, scopes)
  }

  /**
   * @param name the (simple) name of the variable to look up
   * @return true iff the variable is bound in this environment
   */
  has(name: string): boolean {
    return (
      this.locals.some((scope) => scope.has(name)) ||
      this.topLevel.has(name) ||
      [...this.imports.values()].some((lib) => lib.bindings.has(name))
    )
  }

  extendWithImport(name: string, lib: Module): Env {
    return new Env(this.extendImports(name, lib), this.topLevel, this.locals)
  }

  extendWithTopLevel(...bindings: [string, Value][]): Env {
    return new Env(
      this.imports,
      this.extendBindings(this.topLevel, bindings),
      this.locals,
    )
  }

  /** Push a new innermost local scope containing `locals`. */
  extendWithLocals(...locals: [string, Value][]): Env {
    return this.pushScope(locals)
  }

  /** Push a new innermost local scope with the given bindings. */
  pushScope(bindings: [string, Value][]): Env {
    return new Env(this.imports, this.topLevel, [
      ...this.locals,
      new Map(bindings),
    ])
  }

  /** Drop the innermost local scope (inverse of {@link pushScope}). */
  popScope(): Env {
    return new Env(this.imports, this.topLevel, this.locals.slice(0, -1))
  }

  extendImports(name: string, lib: Module) {
    return new Map([...this.imports, [name, lib]])
  }

  extendBindings(old: Map<string, Value>, newBindings: [string, Value][]) {
    return new Map([...old, ...newBindings])
  }

  /** Replace all local scopes with a single scope holding `locals`. */
  extendReplacingLocals(...locals: [string, Value][]): Env {
    return new Env(this.imports, this.topLevel, [new Map(locals)])
  }

  /** Collapse the local scopes into one, dropping the named bindings. */
  withoutLocals(...names: string[]): Env {
    const kept = [...this.getLocals()].filter(([x]) => !names.includes(x))
    return new Env(this.imports, this.topLevel, [new Map(kept)])
  }
}

/** A module is a collection of importable top-level definitions. */
export class Module {
  bindings: Map<string, Value>

  constructor() {
    this.bindings = new Map()
  }

  registerValue(name: string, v: Value) {
    if (typeof v === 'function') {
      Object.defineProperty(v, 'name', { value: name })
    }
    this.bindings.set(name, v)
  }

  static fromLibs(...mods: Module[]): Module {
    const ret = new Module()
    for (const lib of mods) {
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
  [scamperTag]: 'closure'
  params: Id[]
  code: Blk
  // The captured local scope stack, by reference: a binding filled after this
  // closure is created (letrec) is visible through the shared scope objects.
  locals: Scope[]
  restParam?: string
  // N.B., call is required so that Javascript code can call Scamper closures similarly
  // to Javascript functions. Since closures are generated during runtime, the underlying
  // Machine can be referenced by call to perform evaluation.
  call: (...args: Value[]) => Value
  name?: Id
  // When true, a reduction trace steps *over* (not into) a call to this
  // closure: its internal reductions stay hidden and the call reduces to its
  // value atomically. Set for closures defined in imported modules -- the
  // builtin libraries (incl. the prelude) and user file imports -- so a trace
  // steps through the user's own module/local definitions but not library
  // code. See src/scheme/trace.ts.
  stepOver?: boolean
}

/** A char is a tagged object that captures a single character (a one-character string). */
export interface Char extends TaggedObject {
  [scamperTag]: 'char'
  value: string
}

/** A symbol is a tagged object representing an identifier. */
export interface Sym extends TaggedObject {
  [scamperTag]: 'sym'
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
  [scamperTag]: 'struct'
  [structKind]: string
  [key: string]: Value
  [key: number]: never
}

/** A Scamper vector is a Javascript array of values. */
export type Vector = Value[]

/** A Scamper function is either a closure or a raw Javascript function. */
export type JsFunction = (...args: Value[]) => Value
export type ScamperFn = Closure | JsFunction

/**
 * Calls a ScamperFn function with the provided arguments
 * @deprecated We will disallow Javascript code from calling Scamper code
 *             in the near future. Code that uses callScamperFn will need
 *             to be rewritten in Scamper.
 */
export function callScamperFn(_fn: ScamperFn, ..._args: Value[]): Value {
  throw new ScamperError(
    'Runtime',
    'Javascript library functions can no longer call Scamper functions',
  )
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
  [scamperTag]: 'struct'
  [structKind]: 'pair'
  fst: Value
  snd: Value
}

/**
 * A (non-empty) cons cell is an algebraic datatype representing a non-empty list
 * with a head and tail. The tail, itself, must be a list.
 */
export interface Cons extends Struct {
  [scamperTag]: 'struct'
  [structKind]: 'cons'
  head: Value
  tail: List
}

/** A list is either empty (null) or non-empty (cons) */
export type List = null | Cons

///// The Little Pattern Machine language //////////////////////////////////////

// Records that a node was inserted by expanding a derived form (expansion.ts):
// codegen copies it from the AST onto the op, raise copies it back, and sugaring
// uses it to recover the derived form exactly (no heuristics). Undefined on
// nodes that came straight from the parser. `section` is not tracked (it is not
// recovered).
export type Provenance = 'and' | 'or' | 'begin' | 'cond'

export interface Lit {
  tag: 'lit'
  value: Value
  range: Range
  provenance?: Provenance
}
export interface Var {
  tag: 'var'
  name: string
  range: Range
}
export interface Cls {
  tag: 'cls'
  params: string[]
  body: Blk
  name?: string
  range: Range
  restParam?: string
}
export interface Ap {
  tag: 'ap'
  numArgs: number
  range: Range
  provenance?: Provenance
}
export interface Match {
  tag: 'match'
  branches: [Pat, Blk][]
  range: Range
  // hack fix to not modify original branch
  // TODO: making this better requires better bytecode
  currBranchIdx?: number
}
// `let` is letrec: a single scope holds every binder, declared as HOLEs, then
// filled left-to-right as the value sub-blocks evaluate (each may reference any
// binder -- a still-HOLE one is a "referenced before defined" error unless
// deferred in a thunk). `idx` counts the bindings already assigned and is
// threaded through fresh op copies (never mutated in place), so recursion
// through a binding value is re-entrancy-safe. codegen emits a trailing
// `pop-scope`. `if`/`match` linearize their core forms directly too; `match` is
// likewise followed by a `pop-scope`.
export interface Let {
  tag: 'let'
  bindings: { pat: Pat; value: Blk; failMsg?: string }[]
  body: Blk
  range: Range
  // Number of bindings already assigned; 0 on the initial (declaring) run.
  idx: number
  provenance?: Provenance
}
export interface If {
  tag: 'if'
  thenB: Blk
  elseB: Blk
  range: Range
  provenance?: Provenance
}
export interface PopScope {
  tag: 'pop-scope'
  range: Range
}
export interface ApSpread {
  tag: 'ap-spread'
  range: Range
}
// N.B., push-handler/pop-handler bracket the guarded thunk in `with-handler`'s
// closure body. push-handler installs the handler (recording the frame/value-
// stack depth to unwind to); on a raised ScamperError the fiber unwinds to that
// depth and applies the handler (see Fiber.handleError). pop-handler is the
// normal-completion path: it uninstalls the handler and drops the (now-unused)
// handler value, leaving the guarded result on the stack. That with-handler's
// arguments are procedures is enforced by its prelude contract, before
// push-handler runs -- replacing the old check-fn op.
export interface PushHandler {
  tag: 'push-handler'
  range: Range
}
export interface PopHandler {
  tag: 'pop-handler'
  range: Range
}

export type Ops =
  | Lit
  | Var
  | Cls
  | Ap
  | Match
  | Let
  | If
  | PopScope
  | ApSpread
  | PushHandler
  | PopHandler
export type Blk = Ops[]

export interface Disp {
  tag: 'disp'
  expr: Blk
  range: Range
}
export interface Import {
  tag: 'import'
  name: string
  kind: 'builtin' | 'file'
  range: Range
}
export interface Define {
  tag: 'define'
  name: string
  expr: Blk
  range: Range
}
export interface StmtExp {
  tag: 'stmtexp'
  expr: Blk
  range: Range
}
export type Stmt = Disp | Import | Define | StmtExp
export type Prog = Stmt[]

export interface PWild {
  tag: 'pwild'
  range: Range
}
export interface PLit {
  tag: 'plit'
  value: Value
  range: Range
}
export interface PVar {
  tag: 'pvar'
  name: string
  range: Range
}
export interface PCtor {
  tag: 'pctor'
  name: string
  args: Pat[]
  range: Range
}
export type Pat = PWild | PLit | PVar | PCtor
