import { Range } from '../range.js'
import { Id, scamperTag, structKind, Value } from '../value.js'

///// Patterns /////////////////////////////////////////////////////////////////

export type PVar = {
  [scamperTag] : 'struct', [structKind]: 'var',
  id: Id
}
export type PValue = {
  [scamperTag]: 'struct', [structKind]: 'value',
  value: Value
}
export type PCtor = {
  [scamperTag]: 'struct', [structKind]: 'ctor',
  ctor: Id, args: Pat[]
}
export type Pat = PVar | PValue | PCtor

export function mkPVar (id: Id): Pat {
  return { [scamperTag]: 'struct', [structKind]: 'var', id }
}

export function mkPValue (value: Value): Pat {
  return { [scamperTag]: 'struct', [structKind]: 'value', value }
}

export function mkPCtor (ctor: Id, args: Pat[]): Pat {
  return { [scamperTag]: 'struct', [structKind]: 'ctor', ctor, args }
}

///// Expressions //////////////////////////////////////////////////////////////

export type Binding = {
  [scamperTag]: 'struct', [structKind]: 'binding',
  name: Id, value: Expr
}
export type MatchCase = {
  [scamperTag]: 'struct', [structKind]: 'case',
  pattern: Pat, body: Expr
}
export type CondCase = { 
  [scamperTag]: 'struct', [structKind]: 'case',
  guard: Expr, body: Expr
}

export type Var = {
  [scamperTag]: 'var', [structKind]: 'var',
  range?: Range, name: Id
}
export type Int = { 
  [scamperTag]: 'struct', [structKind]: 'int',
  range?: Range, value: number
}
export type Float = {
  [scamperTag]: 'struct', [structKind]: 'float',
  range?: Range, value: number
}
export type Bool = {
  [scamperTag]: 'struct', [structKind]: 'bool', 
  range?: Range, value: boolean
}
export type Null = {
  [scamperTag]: 'struct', [structKind]: 'null', 
  range?: Range
}
export type String = {
  [scamperTag]: 'struct', [structKind]: 'str',
  range?: Range, value: string 
}
export type Char = {
  [scamperTag]: 'struct', [structKind]: 'char', 
  range?: Range, value: string
}
export type Lambda = {
  [scamperTag]: 'struct', [structKind]: 'lambda', 
  range?: Range, params: Id[], body: Expr
}
export type App = {
  [scamperTag]: 'struct', [structKind]: 'app', 
  range?: Range, args: Expr[]
}
export type Let = {
  [scamperTag]: 'struct', [structKind]: 'let',
  range?: Range, isStar: boolean, bindings: Binding[], body: Expr
}
export type And = {
  [scamperTag]: 'struct', [structKind]: 'and', 
  range?: Range, args: Expr[] }
export type Or = { 
  [scamperTag]: 'struct', [structKind]: 'or', 
  range?: Range, args: Expr[]
}
export type If = {
  [scamperTag]: 'struct', [structKind]: 'if',
  range?: Range, guard: Expr, ifBranch: Expr, elseBranch: Expr
}
export type Begin = {
  [scamperTag]: 'struct', [structKind]: 'begin', 
  range?: Range, exprs: Expr[]
}
export type Match = {
  [scamperTag]: 'struct', [structKind]: 'match', 
  range?: Range, cases: MatchCase[]
}
export type Cond = { 
  [scamperTag]: 'struct', [structKind]: 'cond', 
  range?: Range, cases: CondCase[]
}
export type Section = { 
  [scamperTag]: 'struct', [structKind]: 'section', 
  range?: Range, args: Expr[]
}

export type Expr
  = Var | Int | Float | Bool | Null | String | Char
  | Lambda | App | Let | And | Or
  | If | Begin | Match | Cond | Section

export function mkBinding (name: Id, value: Expr): Binding {
  return { [scamperTag]: 'struct', [structKind]: 'binding', name, value }
}

export function mkMatchCase (pattern: Pat, body: Expr): MatchCase {
  return { [scamperTag]: 'struct', [structKind]: 'case', pattern, body }
}

export function mkCondCase (guard: Expr, body: Expr): CondCase {
  return { [scamperTag]: 'struct', [structKind]: 'case', guard, body }
}

export function mkVar (name: Id, range?: Range): Expr {
  return { [scamperTag]: 'var', [structKind]: 'var', name, range }
}

export function mkInt (value: number, range?: Range): Expr { 
  return { [scamperTag]: 'struct', [structKind]: 'int', value, range }
}

export function mkFloat (value: number, range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'float', value, range }
}

export function mkBool (value: boolean, range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'bool', value, range }
}

export function mkNull (range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'null', range }
}

export function mkString (value: string, range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'str', value, range }
}

export function mkChar (value: string, range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'char', value, range }
}

export function mkLambda (params: Id[], body: Expr, range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'lambda', params, body, range }
}

export function mkApp (args: Expr[], range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'app', args, range }
}

export function mkLet (
  isStar: boolean, bindings: Binding[], body: Expr, range?: Range): Expr {
  return {
    [scamperTag]: 'struct', [structKind]: 'let',
    isStar, bindings, body, range
  }
}

export function mkAnd (args: Expr[], range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'and', args, range }
}

export function mkOr (args: Expr[], range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'or', args, range }
}

export function mkIf (
  guard: Expr, ifBranch: Expr, elseBranch: Expr, range?: Range): Expr {
  return {
    [scamperTag]: 'struct', [structKind]: 'if',
    guard, ifBranch, elseBranch, range
  }
}

export function mkBegin (exprs: Expr[], range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'begin', exprs, range }
}

export function mkMatch (cases: MatchCase[], range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'match', cases, range }
}

export function mkCond (cases: CondCase[], range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'cond', cases, range }
}

export function mkSection (args: Expr[], range?: Range): Expr {
  return { [scamperTag]: 'struct', [structKind]: 'section', args, range }
}