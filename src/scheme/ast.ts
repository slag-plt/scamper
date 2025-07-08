import { Range, Value } from '../lpm/runtime.js'
import * as R from '../lpm/runtime.js'

// e ::= n | b | s | c
//     | null | void
//     | (e1 ... ek)
//     | (lambda (x1 ... xk) e)
//     | (let ([x1 e1] ... [xk ek]) e)
//     | (let* ([x1 e1] ... [xk ek]) e)
//     | (and e1 ... ek)
//     | (or e1 ... ek)
//     | (begin e1 ... ek)
//     | (if e1 e2 e3)
//     | (cond [e11 e12] ... [e1k e2k])
//     | (match e [p1 e1] ... [pk ek])
//     | (quote e)
//     | (section e1 ... ek)

////// Query Functions /////////////////////////////////////////////////////////

export function isAtom (v: Value): boolean {
  return !R.isList(R.stripSyntax(v))
}

export function isSexp (v: Value): boolean {
  return R.isList(R.stripSyntax(v))
}

export function isSexpWithHead (v: Value, expected: string): boolean {
  if (!isSexp(v)) { return false }
  const lst = R.stripSyntax(v) as R.List
  if (lst === null) { return false }
  const head = R.stripSyntax(lst.fst)
  return R.isSym(head) && (head as R.Sym).value === expected
}

// Expression forms
export const isLambda = (v: Value) => isSexpWithHead(v, 'lambda')
export const isLet = (v: Value) => isSexpWithHead(v, 'let')
export const isLetStar = (v: Value) => isSexpWithHead(v, 'let*')
export const isAnd = (v: Value) => isSexpWithHead(v, 'and')
export const isOr = (v: Value) => isSexpWithHead(v, 'or')
export const isBegin = (v: Value) => isSexpWithHead(v, 'begin')
export const isIf = (v: Value) => isSexpWithHead(v, 'if')
export const isCond = (v: Value) => isSexpWithHead(v, 'cond')
export const isMatch = (v: Value) => isSexpWithHead(v, 'match')
export const isQuote = (v: Value) => isSexpWithHead(v, 'quote')
export const isSection = (v: Value) => isSexpWithHead(v, 'section')

function asPair (v: Value): { fst: Value, snd: Value } {
  const { range: _range, value } = R.unpackSyntax(v)
  v = value
  return { fst: R.listFirst(v), snd: R.listSecond(v) }
}

export function asLambda (v: Value): { params: Value[], body: Value, range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    params: R.listToVector(R.stripSyntax(R.listSecond(v)) as R.List),
    body: R.listThird(v),
    range
  }
}

export function asLet (v: Value): { bindings: { fst: Value, snd: Value }[], body: Value, range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    bindings: R.listToVector(R.stripSyntax(R.listSecond(v)) as R.List).map(asPair),
    body: R.listThird(v),
    range
  }
}

export function asLetStar (v: Value): { bindings: { fst: Value, snd: Value }[], body: Value, range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    bindings: R.listToVector(R.stripSyntax(R.listSecond(v)) as R.List).map(asPair),
    body: R.listThird(v),
    range
  }
}

export function asAnd (v: Value): { values: Value[], range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    values: R.listToVector(R.stripSyntax(R.listTail(v)) as R.List),
    range
  }
}

export function asOr (v: Value): { values: Value[], range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    values: R.listToVector(R.stripSyntax(R.listTail(v)) as R.List),
    range
  }
}

export function asBegin (v: Value): { values: Value[], range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    values: R.listToVector(R.stripSyntax(R.listTail(v)) as R.List),
    range
  }
}

export function asIf (v: Value): { guard: Value, ifB: Value, elseB: Value, range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    guard: R.listSecond(v),
    ifB: R.listThird(v),
    elseB: R.listFourth(v),
    range
  }
}

export function asCond (v: Value): { clauses: { fst: Value, snd: Value }[], range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    clauses: R.listToVector(R.stripSyntax(R.listSecond(v)) as R.List).map(asPair),
    range
  }
}

export function asMatch (v: Value): { scrutinee: Value, clauses: { fst: Value, snd: Value }[], range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    scrutinee: R.listSecond(v),
    clauses: R.listToVector(R.stripSyntax(R.listThird(v)) as R.List).map(asPair),
    range
  }
}

export function asQuote (v: Value): { values: Value[], range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return { values: R.listToVector(R.stripSyntax(R.listTail(v)) as R.List), range }
}

export function asSection (v: Value): { values: Value[], range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return { values: R.listToVector(R.stripSyntax(R.listTail(v)) as R.List), range }
}

// Statement forms
export const isImport = (v: Value) => isSexpWithHead(v, 'import')
export const isDefine = (v: Value) => isSexpWithHead(v, 'define')
export const isDisplay = (v: Value) => isSexpWithHead(v, 'display')
export const isStruct = (v: Value) => isSexpWithHead(v, 'struct')

export function asImport (v: Value): { name: Value, range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return { name: R.listSecond(v), range }
}

export function asDefine (v: Value): { name: Value, value: Value, range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return { name: R.listSecond(v), value: R.listThird(v), range }
}

export function asDisplay (v: Value): { value: Value, range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return { value: R.listSecond(v), range }
}

export function asStruct (v: Value): { name: Value, fields: Value[], range: Range } {
  const { range, value } = R.unpackSyntax(v)
  v = value
  return {
    name: R.listSecond(v),
    fields: R.listToVector(R.stripSyntax(R.listThird(v)) as R.List),
    range
  }
}