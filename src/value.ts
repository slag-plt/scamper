import { Id, Range, Pat, patToString } from './lang.js'

export type MatchBranch = { pattern: Pat, body: Op[] }

export type Op    = Var | Val | Cls | Ap | If | Let | Seq | Match
export type Var   = { tag: 'var', name: string, range: Range }
export type Val   = { tag: 'val', value: Value }
export type Cls   = { tag: 'cls', params: Id[], ops: Op[] }
export type Ap    = { tag: 'ap', arity: number, range: Range }
export type If    = { tag: 'if', ifb: Op[], elseb: Op[], range: Range }
export type Let   = { tag: 'let', names: Id[] }
export type Seq   = { tag: 'seq', numSubexps: number }
export type Match = { tag: 'match', branches: MatchBranch[], range: Range }

export const mkVar = (name: string, range: Range): Op => ({ tag: 'var', name, range })
export const mkValue = (value: Value): Op => ({ tag: 'val', value })
export const mkCls = (params: Id[], ops: Op[]): Op => ({ tag: 'cls', params, ops })
export const mkAp = (arity: number, range: Range): Op => ({ tag: 'ap', arity, range })
export const mkIf = (ifb: Op[], elseb: Op[], range: Range): Op => ({ tag: 'if', ifb, elseb, range })
export const mkLet = (names: Id[]): Op => ({ tag: 'let', names })
export const mkSeq = (numSubexps: number): Op => ({ tag: 'seq', numSubexps })
export const mkMatch = (branches: MatchBranch[], range: Range): Op => ({ tag: 'match', branches, range })

export function opToString (op: Op): string {
  switch (op.tag) {
    case 'var':
      return `var ${op.name}`
    case 'val':
      return `val ${valueToString(op.value)}`
    case 'cls':
      return `cls (${op.params.join(' ')})`
    case 'ap':
      return `ap ${op.arity}`
    case 'if':
      return `if (${op.ifb.map(opToString).join('; ')}) else (${op.elseb.map(opToString).join('; ')}))`
    case 'let':
      return `let (${op.names.join(' ')})`
    case 'seq':
      return `seq ${op.numSubexps}`
    case 'match':
      return `match (${op.branches.map((b) => patToString(b.pattern) + ' => ' + b.body.map(opToString).join('; ')).join(' | ')})`
  }
}

export type TaggedObject = Closure | Char | Pair | Struct
export type Closure = { _scamperTag: 'closure', params: Id[], ops: Op[], env: Env, name?: string }
export type Char = { _scamperTag: 'char', value: string }
export type Pair = { _scamperTag: 'pair', fst: Value, snd: Value, isList: boolean }
export type Struct = { _scamperTag: 'struct', 'kind': string, 'fields': Value[] }
export type Value = boolean | number | string | null | undefined | Value[] | TaggedObject | Function | Object
export type List = null | Pair

export const isNumber = (v: Value): boolean => typeof v === 'number'
export const isBoolean = (v: Value): boolean => typeof v === 'boolean'
export const isString = (v: Value): boolean => typeof v === 'string'
export const isNull = (v: Value): boolean => v === null
export const isVoid = (v: Value): boolean => v === undefined
export const isArray = (v: Value): boolean => Array.isArray(v)
export const isClosure = (v: Value): boolean => typeof v === 'object' && (v as any)._scamperTag === 'closure'
export const isJsFunction = (v: Value): boolean => typeof v === 'function'
export const isFunction = (v: Value): boolean => isJsFunction(v) || isClosure(v)
export const isChar = (v: Value): boolean => typeof v === 'object' && (v as any)._scamperTag === 'char'
export const isPair = (v: Value): boolean => typeof v === 'object' && (v as any)._scamperTag === 'pair'
export const isList = (v: Value): boolean => v === null || (isPair(v) && (v as Pair).isList)
export const isStruct = (v: Value): boolean => typeof v === 'object' && (v as any)._scamperTag === 'struct'
export const isStructKind = (v: Value, k: string): boolean => isStruct(v) && (v as Struct).kind === k

export const mkClosure = (arity: number, params: Id[], ops: Op[], env: Env): Value => ({ _scamperTag: 'closure', arity, params, ops, env })
export const mkChar = (v: string): Char => ({ _scamperTag: 'char', value: v })
export const mkPair = (fst: Value, snd: Value): Pair => ({
  _scamperTag: 'pair', fst, snd,
  isList: snd === null || (isPair(snd) && (snd as Pair).isList)
})
export const mkStruct = (kind: string, fields: Value[]): Value => ({ _scamperTag: 'struct', kind, fields })

export const nameFn = (name: string, fn: Function): Function =>
  Object.defineProperty(fn, 'name', { value: name })

export function listToArray (l: List): Value[] {
  const ret = []
  let cur = l
  while (cur !== null) {
    ret.push(cur.fst)
    cur = cur.snd as Pair
  }
  return ret
}

export function arrayToList (arr: Value[]): Pair | null {
  let ret = null
  for (let i = arr.length - 1; i >= 0; i--) {
    ret = mkPair(arr[i], ret)
  }
  return ret
}

export function valueToString (v: Value) {
  if (isClosure(v)) {
    return `<closure (${(v as Closure).params.join(' ')})>`
  } else if (typeof v === 'function') {
    return `<jsfunc: (${v.name})>`
  } else {
    return `${v}`
  }
}

export function valuesEqual (v1: Value, v2: Value): boolean {
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
    return valuesEqual((v1 as Pair).fst, (v2 as Pair).fst) &&
      valuesEqual((v1 as Pair).snd, (v2 as Pair).snd)
  } else if (isChar(v1) && isChar(v2)) {
    return (v1 as Char).value === (v2 as Char).value
  } else if (isStruct(v1) && isStruct(v2)) {
    const s1 = v1 as Struct
    const s2 = v2 as Struct
    return s1.kind === s2.kind && s1.fields.length === s2.fields.length &&
      s1.fields.every((f, i) => valuesEqual(f, s2.fields[i]))
  } else {
    return false
  }
}

export function typeOfValue (v: Value): string {
  const t = typeof v
  if (t === 'boolean' || t === 'number' || t === 'string') { return t }
  if (v === 'null') { return 'null' }
  if (v === 'undefined') { return 'void'}
  if (Array.isArray(v)) { return 'vector' }
  if (isChar(v)) { return 'char' }
  if (isList(v)) { return 'list' }
  if (isStruct(v)) { return `struct (${(v as Struct).kind})` }
  if (t === 'function' || isClosure(v)) { return 'function' }
  return 'object'
}

export class Env {
  private bindings: Map<Id, Value>
  private parent?: Env

  constructor (bindings: [Id, Value][], parent?: Env) {
    this.bindings = new Map(bindings)
    this.parent = parent
  }

  has (name: Id): boolean {
    return this.bindings.has(name) ||
      (this.parent !== undefined && this.parent.has(name))
  }

  get (name: Id): Value | undefined {
    if (this.bindings.has(name)) {
      return this.bindings.get(name)
    } else if (this.parent !== undefined && this.parent.has(name)) {
      return this.parent.get(name)
    }
  }

  extend (bindings: [Id, Value][]): Env {
    return new Env(bindings, this)
  }

  set (name: Id, v: Value): void {
    this.bindings.set(name, v)
  }
}