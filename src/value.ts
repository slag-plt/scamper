import { Id, Range } from './lang.js'

export type Op   = Var | Val | Cls | Ap | If | Let | Disp
export type Var  = { tag: 'var', name: string, range: Range }
export type Val  = { tag: 'val', value: Value }
export type Cls  = { tag: 'cls', params: Id[], ops: Op[] }
export type Ap   = { tag: 'ap', arity: number, range: Range }
export type If   = { tag: 'if', ifb: Op[], elseb: Op[], range: Range }
export type Let  = { tag: 'let', names: Id[] }
export type Disp = { tag: 'disp' }

export const mkVar = (name: string, range: Range): Op => ({ tag: 'var', name, range })
export const mkValue = (value: Value): Op => ({ tag: 'val', value })
export const mkCls = (params: Id[], ops: Op[]): Op => ({ tag: 'cls', params, ops })
export const mkAp = (arity: number, range: Range): Op => ({ tag: 'ap', arity, range })
export const mkIf = (ifb: Op[], elseb: Op[], range: Range): Op => ({ tag: 'if', ifb, elseb, range })
export const mkLet = (names: Id[]): Op => ({ tag: 'let', names })
export const mkDisp = (): Op => ({ tag: 'disp' })

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
    case 'disp':
      return 'disp'
  }
}

export type TaggedObject = Closure | JsFunction | Char | Pair | Struct
export type Closure = { _scamperTag: 'closure', params: Id[], ops: Op[], env: Env }
export type JsFunction = { _scamperTag: 'jsfunc', fn: Function, arity: number, isVariadic: boolean }
export type Char = { _scamperTag: 'char', value: string }
export type Pair = { _scamperTag: 'pair', fst: Value, snd: Value, isList: boolean }
export type Struct = { _scamperTag: 'struct', 'kind': string, 'fields': Value[] }
export type Value = boolean | number | string | null | undefined | Value[] | TaggedObject | Function | Object
export type List = null | Pair

export const isClosure = (v: Value): boolean => typeof v === 'object' && (v as any)._scamperTag === 'closure'
export const isJsFunction = (v: Value): boolean => typeof v === 'object' && (v as any)._scamperTag === 'jsfunc'
export const isChar = (v: Value): boolean => typeof v === 'object' && (v as any)._scamperTag === 'char'
export const isPair = (v: Value): boolean => typeof v === 'object' && (v as any)._scamperTag === 'pair'
export const isStruct = (v: Value): boolean => typeof v === 'object' && (v as any)._scamperTag === 'struct'
export const isStructKind = (v: Value, k: string): boolean => isStruct(v) && (v as Struct).kind === k

export const mkClosure = (arity: number, params: Id[], ops: Op[], env: Env): Value => ({ _scamperTag: 'closure', arity, params, ops, env })
export const mkJsFunction = (fn: Function, arity: number, isVariadic: boolean = false): Value => ({ _scamperTag: 'jsfunc', fn, arity, isVariadic })
export const mkChar = (v: string): Char => ({ _scamperTag: 'char', value: v })
export const mkPair = (fst: Value, snd: Value): Pair => ({
  _scamperTag: 'pair', fst, snd,
  isList: snd === null || (isPair(snd) && (snd as Pair).isList)
})
export const mkStruct = (kind: string, fields: Value[]): Value => ({ _scamperTag: 'struct', kind, fields })

export function valueToString (v: Value) {
  if (isClosure(v)) {
    return `<closure (${(v as Closure).params.join(' ')})>`
  } else if (isJsFunction(v)) {
    return `<jsfunc (${(v as JsFunction).arity})>`
  } else {
    return `${v}`
  }
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