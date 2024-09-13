import { Expr } from './expr.js'
import { Range } from '../range.js'
import { Id, scamperTag, structKind } from '../value.js'

export type Binding = {
  [scamperTag]: 'struct', [structKind]: 'binding',
  range?: Range, name: Id, value: Expr
}

export type StmtExpr = {
  [scamperTag]: 'struct', [structKind]: 'exp',
  range?: Range, exp: Expr
}

export type Import = {
  [scamperTag]: 'struct', [structKind]: 'import',
  range?: Range, path: string
}

export type Display = { 
  [scamperTag]: 'struct', [structKind]: 'display',
  range?: Range, expr: Expr
}

export type Struct = { 
  [scamperTag]: 'struct', [structKind]: 'struct',
  range?: Range, name: Id, fields: Binding[]
}

export function mkBinding (name: Id, value: Expr, range?: Range): Binding {
  return {
    [scamperTag]: 'struct', [structKind]: 'binding',
    name, value, range
  }
}

export function mkStmtExpr (exp: Expr, range?: Range): StmtExpr {
  return {
    [scamperTag]: 'struct', [structKind]: 'exp',
    exp, range
  }
}

export function mkImport (path: string, range?: Range): Import {
  return {
    [scamperTag]: 'struct', [structKind]: 'import',
    path, range
  }
}

export function mkDisplay (expr: Expr, range?: Range): Display {
  return {
    [scamperTag]: 'struct', [structKind]: 'display',
    expr, range
  }
}

export function mkStruct (name: Id, fields: Binding[], range?: Range): Struct {
  return {
    [scamperTag]: 'struct', [structKind]: 'struct',
    name, fields, range
  }
}

export type Stmt = Binding | StmtExpr | Import | Display | Struct

export type Program = Stmt[]