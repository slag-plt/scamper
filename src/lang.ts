import {AST} from "./ast";

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
}

export class Range {
  begin: Loc
  end: Loc

  constructor (startLine: number, startCol: number, startIdx: number, endLine: number, endCol: number, endIdx: number) {
    this.begin = new Loc(startLine, startCol, startIdx)
    this.end = new Loc(endLine, endCol, endIdx)
  }

  public toString (): string {
    return `${this.begin.toString()}-${this.end.toString()}`
  }
}

export const mkRange = (beg: Loc, end: Loc): Range => new Range(beg.line, beg.col, beg.idx, end.line, end.col, end.idx)

export const noLoc = new Loc(-1, -1, -1)
export const noRange = new Range(-1, -1, -1, -1, -1, -1)

type Phase = 'Parser' | 'Runtime'

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
    const detail = `${this.modName ? this.modName : ''}${(this.range && this.range !== noRange) ? this.range.toString() : ''}`
    const src = this.source ? `(${this.source}) ` : ''
    return `${this.phase} error${detail.length > 0 ? ' [' + detail + ']' : ''}: ${src}${this.message}`
  }
}

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

export type Id = string

const specialCharToNameMap: Map<string, string> = new Map([
  [String.fromCharCode(9), 'alarm'],
  [String.fromCharCode(7), 'backspace'],
  [String.fromCharCode(126), 'delete'],
  [String.fromCharCode(26), 'escape'],
  [String.fromCharCode(9), 'newline'],
  [String.fromCharCode(-1), 'null'],
  [String.fromCharCode(12), 'return'],
  [' ', 'space'],
  [String.fromCharCode(8), 'tab']
])

export function charToName (c: string): string {
  if (specialCharToNameMap.has(c)) {
    return specialCharToNameMap.get(c)!
  } else {
    return c
  }
}

export const reservedWords = [
  'and',
  'begin',
  'cond',
  'define',
  'if',
  'import',
  'lambda',
  'let',
  'let*',
  'letrec',
  'match',
  'or',
  'quote',
  'section',
  'struct',
]

export namespace Value {
  export const scamperTag = Symbol('tag')
  export const structKind = Symbol('kind')

  export type TaggedObject = Closure | Char | Sym | Pair | Syntax | Struct
  export type Closure = { [scamperTag]: 'closure', params: Id[], ops: Op.T[], env: Env, name?: string }
  export type Char = { [scamperTag]: 'char', value: string }
  export type Sym  = { [scamperTag]: 'sym', value: string } 
  export type Pair = { [scamperTag]: 'pair', fst: T, snd: T, isList: boolean }
  export type Syntax = { [scamperTag]: 'syntax', range: Range, value: T }

  // NOTE: to maximize interoperability, a struct is an object with at least
  // a _scamperTag and kind field. The rest of the fields are the fields of the
  // the struct. The order of arguments of a struct's constructor is the
  // property order of the corresponding object, i.e., the order in which the
  // fields are defined.
  export interface Struct { [scamperTag]: 'struct', [structKind]: string, [key: string]: any, [key: number]: never }

  export type List = null | Pair
  export type Vector = T[]
  export type ScamperFn = Closure | Function
  export type Raw = Object
  export type T = boolean | number | string | List | Vector | Function | undefined | TaggedObject | Raw

  export const isNumber = (v: T): boolean => typeof v === 'number'
  export const isBoolean = (v: T): boolean => typeof v === 'boolean'
  export const isString = (v: T): boolean => typeof v === 'string'
  export const isNull = (v: T): boolean => v === null
  export const isVoid = (v: T): boolean => v === undefined
  export const isArray = (v: T): boolean => Array.isArray(v)
  export const isJsFunction = (v: T): boolean => typeof v === 'function'

  export const isTaggedObject = (v: T): boolean =>
    v !== null && typeof v === 'object' && v.hasOwnProperty(scamperTag)
  export const isClosure = (v: T): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'closure'
  export const isFunction = (v: T): boolean => isJsFunction(v) || isClosure(v)
  export const isChar = (v: T): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'char'
  export const isSym = (v: T): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'sym'
  export const isSymName = (v: T, name: string): boolean => isSym(v) && (v as Sym).value === name
  export const isPair = (v: T): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'pair'
  export const isList = (v: T): boolean => v === null || (isPair(v) && (v as Pair).isList)
  export const isSyntax = (v: T): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'syntax'
  export const isStruct = (v: T): boolean => isTaggedObject(v) && (v as TaggedObject)[scamperTag] === 'struct'
  export const isStructKind = (v: T, k: string): boolean => isStruct(v) && (v as Struct)[structKind] === k

  export const mkClosure = (arity: number, params: Id[], ops: Op.T[], env: Env): T =>
    ({ [scamperTag]: 'closure', arity, params, ops, env })
  export const mkChar = (v: string): Char => ({ [scamperTag]: 'char', value: v })
  export const mkSym = (v: string): Sym => ({ [scamperTag]: 'sym', value: v })
  export const mkPair = (fst: T, snd: T): Pair => ({
    [scamperTag]: 'pair', fst, snd,
    isList: snd === null || ((isPair(snd) && (snd as Pair).isList))
  })
  export const mkList = (...values: T[]): List => vectorToList(values)
  export const mkSyntax = (range: Range, value: T): Syntax =>
    ({ [scamperTag]: 'syntax', range, value })
  export const mkStruct = (kind: string, fields: string[], values: T[]): T => {
    const ret: Struct = { [scamperTag]: 'struct', [structKind]: kind }
    for (let i = 0; i < fields.length; i++) {
      ret[fields[i]] = values[i]
    }
    return ret
  }

  export const stripSyntax = (v: T): T =>
    isSyntax(v) ? (v as Syntax).value : v
  
  export function stripAllSyntax (v: T): T {
    if (isSyntax(v)) {
      return stripAllSyntax((v as Syntax).value)
    } else if (isPair(v)) {
      const p = v as Pair
      return mkPair(stripAllSyntax(p.fst), stripAllSyntax(p.snd))
    } else if (isArray(v)) {
      return (v as T[]).map(stripAllSyntax)
    } else if (isStruct(v)) {
      const s = v as Struct
      const fields = getFieldsOfStruct(s)
      return mkStruct(s[structKind], fields, fields.map((f) => stripAllSyntax(s[f])))
    } else {
      return v
    }
  }

  export const unpackSyntax = (v: T): { range: Range, value: T } =>
    isSyntax(v) ?
      { range: (v as Syntax).range, value: (v as Syntax).value } :
      { range: noRange, value: v }

  export const rangeOf = (v: T): Range =>
    isSyntax(v) ? (v as Syntax).range : noRange

  export const nameFn = (name: string, fn: Function): Function =>
    Object.defineProperty(fn, 'name', { value: name })

  export function listToVector (l: List): T[] {
    const ret = []
    let cur = l
    while (cur !== null) {
      ret.push(cur.fst)
      cur = cur.snd as Pair
    }
    return ret
  }

  export function vectorToList (arr: T[]): Pair | null {
    let ret = null
    for (let i = arr.length - 1; i >= 0; i--) {
      ret = mkPair(arr[i], ret)
    }
    return ret
  }

  export function toString (v: T) {
    if (isClosure(v)) {
      return `<closure (${(v as Closure).params.join(' ')})>`
    } else if (typeof v === 'function') {
      return `<jsfunc: (${v.name})>`
    } else {
      return `${v}`
    }
  }

  export function getFieldsOfStruct (s: Struct): string[] {
    const ret = []
    for (const f in s) {
      ret.push(f)
    }
    return ret
  }

  export function equal (v1: T, v2: T): boolean {
    const t1 = typeof v1
    const t2 = typeof v2
    if ((v1 === null && v2 === null) ||
        (v1 === undefined && v2 === undefined)) {
          return true
    } else if ((t1 === 'number' && t2 === 'number') ||
        (t1 === 'boolean' && t2 === 'boolean') ||
        (t1 === 'string' && t2 === 'string') ||
        (t1 === 'undefined' && t2 === 'undefined') ||
        // N.B., function equality is reference-ish equality
        (isFunction(t1) && isFunction(t2))) {
      return v1 === v2
    } else if (isPair(v1) && isPair(v2)) {
      return equal((v1 as Pair).fst, (v2 as Pair).fst) &&
        equal((v1 as Pair).snd, (v2 as Pair).snd)
    } else if (isChar(v1) && isChar(v2)) {
      return (v1 as Char).value === (v2 as Char).value
    } else if (isArray(v1) && isArray(v2)) {
      const a1 = v1 as T[]
      const a2 = v2 as T[]
      return a1.length === a2.length && a1.every((v, i) => equal(v, a2[i]))
    } else if (isStruct(v1) && isStruct(v2)) {
      const s1 = v1 as Struct
      const s2 = v2 as Struct
      const fieldsS1 = getFieldsOfStruct(s1)
      const fieldsS2 = getFieldsOfStruct(s2)
      return s1[Value.structKind] === s2[Value.structKind] && fieldsS1.length === fieldsS2.length &&
        fieldsS1.every((f) => equal(s1[f], s2[f]))
    } else {
      return false
    }
  }

  export function typeOf (v: T): string {
    const t = typeof v
    if (t === 'boolean' || t === 'number' || t === 'string') { return t }
    if (v === 'null') { return 'null' }
    if (v === 'undefined') { return 'void'}
    if (Array.isArray(v)) { return 'vector' }
    if (isChar(v)) { return 'char' }
    if (isList(v)) { return 'list' }
    if (isStruct(v)) { return `struct (${(v as Struct)[Value.structKind].toString()})` }
    if (t === 'function' || isClosure(v)) { return 'function' }
    return 'object'
  }
}

export namespace Stmt {
  export type T = Binding | Exp | Import | Display | Struct
  export type Binding = { _scamperTag: 'struct', kind: 'binding', name: Id, body: Op.T[], src: Value.T, range: Range }
  export type Exp = { _scamperTag: 'struct', kind: 'exp', body: Op.T[], src: Value.T, range: Range }
  export type Import = { _scamperTag: 'struct', kind: 'import', modName: string, range: Range }
  export type Display = { _scamperTag: 'struct', kind: 'display', body: Op.T[], src: Value.T, range: Range }
  export type Struct = { _scamperTag: 'struct', kind: 'struct', id: string, fields: string[], range: Range }

  export const mkStmtBinding = (name: Id, body: Op.T[], src: Value.T, range: Range): T =>
    ({ _scamperTag: 'struct', kind: 'binding', name, body, src, range })
  export const mkStmtExp = (body: Op.T[], src: Value.T, range: Range): T => ({ _scamperTag: 'struct', kind: 'exp', body, src, range })
  export const mkImport = (modName: string, range: Range): T => ({ _scamperTag: 'struct', kind: 'import', modName, range })
  export const mkDisplay = (body: Op.T[], src: Value.T, range: Range): T => ({ _scamperTag: 'struct', kind: 'display', body, src, range })
  export const mkStruct = (id: string, fields: string[], range: Range): T => ({ _scamperTag: 'struct', kind: 'struct', id, fields, range })
}

export type Prog = Stmt.T[]
export type ParserOutput = {prog: Prog, ast: AST}

export namespace Op {
  export type Label = string
  export const freshLabel: () => Label = (() => {
    let counter = 0
    return () => `lbl_${counter++}`
  })()

  export type MatchBranch = { pattern: Value.T, body: T[] }

  export type T      = Var | Val | Cls | Ap | If | Let | Seq | Match | And | Or | Cond | Lbl | Exn
  export type Var    = { tag: 'var', name: string, range: Range }
  export type Val    = { tag: 'val', value: Value.T }
  export type Cls    = { tag: 'cls', params: Id[], ops: T[] }
  export type Ap     = { tag: 'ap', arity: number, range: Range }
  export type If     = { tag: 'if', ifb: T[], elseb: T[], range: Range }
  export type Let    = { tag: 'let', names: Id[], body: T[] }
  export type Seq    = { tag: 'seq', numSubexps: number }
  export type Match  = { tag: 'match', branches: MatchBranch[], range: Range }
  export type And    = { tag: 'and', jmpTo: Label, range: Range }
  export type Or     = { tag: 'or', jmpTo: Label, range: Range }
  export type Cond   = { tag: 'cond', body: T[], end: Label, range: Range }
  export type Lbl    = { tag: 'lbl', name: string }
  export type Exn    = { tag: 'exn', msg: string, modName?: string, range?: Range, source?: string }

  export const mkVar = (name: string, range: Range): T => ({ tag: 'var', name, range })
  export const mkValue = (value: Value.T): T => ({ tag: 'val', value })
  export const mkCls = (params: Id[], ops: T[]): T => ({ tag: 'cls', params, ops })
  export const mkAp = (arity: number, range: Range): T => ({ tag: 'ap', arity, range })
  export const mkIf = (ifb: T[], elseb: T[], range: Range): T => ({ tag: 'if', ifb, elseb, range })
  export const mkLet = (names: Id[], body: T[]): T => ({ tag: 'let', names, body })
  export const mkSeq = (numSubexps: number): T => ({ tag: 'seq', numSubexps })
  export const mkMatch = (branches: MatchBranch[], range: Range): T => ({ tag: 'match', branches, range })
  export const mkAnd = (jmpTo: Label, range: Range): T => ({ tag: 'and', jmpTo, range })
  export const mkOr = (jmpTo: Label, range: Range): T => ({ tag: 'or', jmpTo, range })
  export const mkCond = (body: T[], end: Label, range: Range): T => ({ tag: 'cond', body, end, range })
  export const mkLbl = (name: string): T => ({ tag: 'lbl', name })
  export const mkExn = (msg: string, modName?: string, range?: Range, source?: string): T => ({ tag: 'exn', msg, modName, range, source })
}

export class Env {
  private bindings: Map<Id, Value.T>
  private parent?: Env

  constructor (bindings: [Id, Value.T][], parent?: Env) {
    this.bindings = new Map(bindings)
    this.parent = parent
  }

  has (name: Id): boolean {
    return this.bindings.has(name) ||
      (this.parent !== undefined && this.parent.has(name))
  }

  get (name: Id): Value.T | undefined {
    if (this.bindings.has(name)) {
      return this.bindings.get(name)
    } else if (this.parent !== undefined && this.parent.has(name)) {
      return this.parent.get(name)
    }
  }

  extend (bindings: [Id, Value.T][]): Env {
    return new Env(bindings, this)
  }

  set (name: Id, v: Value.T): void {
    this.bindings.set(name, v)
  }

  remove (...names: Id[]): void {
    for (const n of names) {
      this.bindings.delete(n)
    }
    this.parent?.remove(...names)
  }

  clone (): Env {
    return new Env([...this.bindings.entries()], this.parent?.clone())
  }

  quotient (...names: Id[]): Env {
    const ret = this.clone()
    ret.remove(...names)
    return ret 
  }
}

export type Library = {
  lib: [string, Value.T][],
  initializer: Function | undefined
}

export function emptyLibrary(): Library {
  return { lib: [], initializer: undefined }
}

export function registerValue (name: string, v: Value.T, library: Library) {
  if (typeof v === 'function') {
    Value.nameFn(name, v)
  }
  library.lib.push([name, v])
}
