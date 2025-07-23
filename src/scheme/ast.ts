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

///// Syntax Wrappers //////////////////////////////////////////////////////////

export interface Syntax extends R.Struct {
  [R.scamperTag]: 'struct'
  [R.structKind]: 'syntax'
  range: Range
  value: Value
  metadata?: Map<string, any>
}

export const mkSyntax = (range: Range, value: Value, ...metadata: [string, any][]): Syntax =>
  ({ [R.scamperTag]: 'struct', [R.structKind]: 'syntax', range, value, metadata: new Map(metadata) })

export const isSyntax = (v: Value): v is Syntax => R.isStructKind(v, 'syntax')

/** @return v but with its top-level syntax wrapper removed, if it exists. */
export const stripSyntax = (v: Value): Value => isSyntax(v) ? v.value : v

/** @return v but with all syntax wrappers removed, recursively. */
export function stripAllSyntax (v: Value): Value {
  if (isSyntax(v)) {
    return stripAllSyntax((v as Syntax).value)
  } else if (R.isPair(v)) {
    return R.mkPair(stripAllSyntax(v.fst), stripAllSyntax(v.snd))
  } else if (R.isArray(v)) {
    return v.map(stripAllSyntax)
  } else if (R.isStruct(v)) {
    const fields = R.getFieldsOfStruct(v)
    return R.mkStruct(v[R.structKind], fields, fields.map((f) => stripAllSyntax(v[f])))
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
export const rangeOf = (v: Value, fallback: Range = Range.none): Range =>
  isSyntax(v) ? (v as Syntax).range : fallback

////// Query Functions /////////////////////////////////////////////////////////

export function isAtom (v: Value): boolean {
  return !R.isList(stripSyntax(v))
}

export function isApp (v: Value): boolean {
  return R.isList(stripSyntax(v))
}

export function isSpecialForm (v: Value, expected: string): boolean {
  if (!isApp(v)) { return false }
  const lst = stripSyntax(v) as R.List
  if (lst === null) { return false }
  const head = stripSyntax(lst.fst)
  return R.isSym(head) && (head as R.Sym).value === expected
}

// Special Forms
export const isLambda = (v: Value) => isSpecialForm(v, 'lambda')
export const isLet = (v: Value) => isSpecialForm(v, 'let')
export const isLetStar = (v: Value) => isSpecialForm(v, 'let*')
export const isAnd = (v: Value) => isSpecialForm(v, 'and')
export const isOr = (v: Value) => isSpecialForm(v, 'or')
export const isBegin = (v: Value) => isSpecialForm(v, 'begin')
export const isIf = (v: Value) => isSpecialForm(v, 'if')
export const isCond = (v: Value) => isSpecialForm(v, 'cond')
export const isMatch = (v: Value) => isSpecialForm(v, 'match')
export const isQuote = (v: Value) => isSpecialForm(v, 'quote')
export const isSection = (v: Value) => isSpecialForm(v, 'section')

export type Pair = { fst: Value, snd: Value, range: Range }

function asPair (v: Value): Pair {
  const { range: range, value } = unpackSyntax(v)
  v = value
  return { fst: R.listFirst(v), snd: R.listSecond(v), range }
}

export function asApp (v: Value): { values: Value[], range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    values: R.listToVector(v as R.List),
    range
  }
}

export function asLambda (v: Value): { params: Value[], body: Value, range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    params: R.listToVector(stripSyntax(R.listSecond(v)) as R.List),
    body: R.listThird(v),
    range
  }
}

export function asLet (v: Value): { bindings: Pair[], body: Value, range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    bindings: R.listToVector(stripSyntax(R.listSecond(v)) as R.List).map(asPair),
    body: R.listThird(v),
    range
  }
}

export function asLetStar (v: Value): { bindings: Pair[], body: Value, range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    bindings: R.listToVector(stripSyntax(R.listSecond(v)) as R.List).map(asPair),
    body: R.listThird(v),
    range
  }
}

export function asAnd (v: Value): { values: Value[], range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    values: R.listToVector(stripSyntax(R.listTail(v)) as R.List),
    range
  }
}

export function asOr (v: Value): { values: Value[], range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    values: R.listToVector(stripSyntax(R.listTail(v)) as R.List),
    range
  }
}

export function asBegin (v: Value): { values: Value[], range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    values: R.listToVector(stripSyntax(R.listTail(v)) as R.List),
    range
  }
}

export function asIf (v: Value): { guard: Value, ifB: Value, elseB: Value, range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    guard: R.listSecond(v),
    ifB: R.listThird(v),
    elseB: R.listFourth(v),
    range
  }
}

export function asCond (v: Value): { clauses: Pair[], range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    clauses: R.listToVector(stripSyntax(R.listSecond(v)) as R.List).map(asPair),
    range
  }
}

export function asMatch (v: Value): { scrutinee: Value, clauses: Pair[], range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    scrutinee: R.listSecond(v),
    clauses: R.listToVector(stripSyntax(R.listThird(v)) as R.List).map(asPair),
    range
  }
}

export function asQuote (v: Value): { values: Value[], range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return { values: R.listToVector(stripSyntax(R.listTail(v)) as R.List), range }
}

export function asSection (v: Value): { values: Value[], range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return { values: R.listToVector(stripSyntax(R.listTail(v)) as R.List), range }
}

// Statement forms
export const isImport = (v: Value) => isSpecialForm(v, 'import')
export const isDefine = (v: Value) => isSpecialForm(v, 'define')
export const isDisplay = (v: Value) => isSpecialForm(v, 'display')
export const isStruct = (v: Value) => isSpecialForm(v, 'struct')

export function asImport (v: Value): { name: Value, range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return { name: R.listSecond(v), range }
}

export function asDefine (v: Value): { name: Value, value: Value, range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return { name: R.listSecond(v), value: R.listThird(v), range }
}

export function asDisplay (v: Value): { value: Value, range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return { value: R.listSecond(v), range }
}

export function asStruct (v: Value): { name: Value, fields: Value[], range: Range } {
  const { range, value } = unpackSyntax(v)
  v = value
  return {
    name: R.listSecond(v),
    fields: R.listToVector(stripSyntax(R.listThird(v)) as R.List),
    range
  }
}