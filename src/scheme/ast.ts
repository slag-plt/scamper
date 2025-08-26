import { Value } from '../lpm'
import * as L from '../lpm'

// e ::= n | b | s | c
//     | null | void
//     | (e1 ... ek)
//
//     -- Special forms
//     | (lambda (x1 ... xk) e)
//     | (let ([x1 e1] ... [xk ek]) e)
//     | (begin e1 ... ek)
//     | (if e1 e2 e3)
//     | (match e [p1 e1] ... [pk ek])
//     | (quote e)
//
//     -- Sugared forms
//     | (let* ([x1 e1] ... [xk ek]) e)
//     | (and e1 ... ek)
//     | (or e1 ... ek)
//     | (cond [e11 e12] ... [e1k e2k])
//     | (section e1 ... ek)
//
// s ::= e
//     | (import m)
//     | (define x e)
//     | (display e)
//     | e
//
//     -- Sugared form
//     | (struct S (f1 ... fk))

///// Syntax Wrappers //////////////////////////////////////////////////////////

export type Metadata = Map<string, any>
export type MetadataEntry = [string, any]

/**
 * A syntax value wraps a value that serves as a Scheme AST. It provides
 * metadata information, e.g., source ranges, for the underlying AST.
 */
export interface Syntax extends L.Struct {
  [L.scamperTag]: 'struct'
  [L.structKind]: 'syntax'
  metadata: Metadata
  value: Value
}

export const mkSyntax = (value: Value, ...metadata: MetadataEntry[]): Syntax =>
  ({ [L.scamperTag]: 'struct', [L.structKind]: 'syntax', metadata: new Map(metadata), value })

export const isSyntax = (v: Value): v is Syntax => L.isStructKind(v, 'syntax')

/** @return v but with its top-level syntax wrapper removed, if it exists. */
export const stripSyntax = (v: Value): Value => isSyntax(v) ? v.value : v

/** @return v but with all syntax wrappers removed, recursively. */
export function stripAllSyntax (v: Value): Value {
  if (isSyntax(v)) {
    return stripAllSyntax((v as Syntax).value)
  } else if (L.isList(v)) {
    return L.mkList(...L.listToVector(v).map(stripAllSyntax))
  } else if (L.isPair(v)) {
    return L.mkPair(stripAllSyntax(v.fst), stripAllSyntax(v.snd))
  } else if (L.isArray(v)) {
    return v.map(stripAllSyntax)
  } else if (L.isStruct(v)) {
    const fields = L.getFieldsOfStruct(v)
    return L.mkStruct(v[L.structKind], fields, fields.map((f) => stripAllSyntax(v[f])))
  } else {
    return v
  }
}

/**
 * @return a pair of a value and its associated metadata if it is a syntax object
 *         or a fresh metadata map if it is a raw value.
 */
export const unpackSyntax = (v: Value): { value: Value, metadata: Metadata } =>
  isSyntax(v) ?
    { value: v.value, metadata: v.metadata } :
    { value: v, metadata: new Map() }

////// Query Functions /////////////////////////////////////////////////////////

export function isAtom (v: Value): boolean {
  return !L.isList(stripSyntax(v))
}

export function isApp (v: Value): boolean {
  return L.isList(stripSyntax(v))
}

export function isIdentifier (v: Value): boolean {
  return isAtom(v) && L.isSym(stripSyntax(v))
}

export function isIdentifierNamed (v: Value, s: string): boolean {
  return isIdentifier(v) && (stripSyntax(v) as L.Sym).value === s
}

export function asIdentifier (v: Value): { name: string, metadata: Metadata } {
  const { value, metadata } = unpackSyntax(v)
  v = value
  return {
    name: (v as L.Sym).value,
    metadata
  }
}

export function nameFromIdentifier (v: Value): string {
  const { name, metadata: _metadata } = asIdentifier(v)
  return name
}

export function isSpecialForm (v: Value, expected: string): boolean {
  if (!isApp(v)) { return false }
  const lst = stripSyntax(v) as L.List
  if (lst === null) { return false }
  const head = stripSyntax(lst.head)
  return L.isSym(head) && (head as L.Sym).value === expected
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

export type Pair = { fst: Value, snd: Value, metadata: Metadata }

function asPair (v: Value): Pair {
  const { value, metadata } = unpackSyntax(v)
  v = value
  return { fst: L.listNth(0, v as L.List), snd: L.listNth(1, v as L.List), metadata }
}

export function asApp (v: Value): { values: Value[], metadata: Metadata } {
  const { value, metadata } = unpackSyntax(v)
  v = value
  return {
    values: L.listToVector(v as L.List),
    metadata
  }
}

export function asLambda (v: Value): { params: Value[], body: Value, metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    params: L.listToVector(stripSyntax(L.listNth(1, v as L.List)) as L.List),
    body: L.listNth(2, v as L.List),
    metadata
  }
}

export function asLet (v: Value): { bindings: Pair[], body: Value, metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    bindings: L.listToVector(stripSyntax(L.listNth(1, v as L.List)) as L.List).map(asPair),
    body: L.listNth(2, v as L.List),
    metadata
  }
}

export function asLetStar (v: Value): { bindings: Pair[], body: Value, metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    bindings: L.listToVector(stripSyntax(L.listNth(1, v as L.List)) as L.List).map(asPair),
    body: L.listNth(2, v as L.List),
    metadata
  }
}

export function asAnd (v: Value): { values: Value[], metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    values: L.listToVector(stripSyntax((v as L.List)!.tail) as L.List),
    metadata
  }
}

export function asOr (v: Value): { values: Value[], metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    values: L.listToVector(stripSyntax((v as L.List)!.tail) as L.List),
    metadata
  }
}

export function asBegin (v: Value): { values: Value[], metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    values: L.listToVector(stripSyntax((v as L.List)!.tail) as L.List),
    metadata
  }
}

export function asIf (v: Value): { guard: Value, ifB: Value, elseB: Value, metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    guard: L.listNth(1, v as L.List),
    ifB: L.listNth(2, v as L.List),
    elseB: L.listNth(3, v as L.List),
    metadata
  }
}

export function asCond (v: Value): { clauses: Pair[], metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    clauses: L.listToVector((v as L.Pair).tail as L.List).map(asPair),
    metadata
  }
}

export function asMatch (v: Value): { scrutinee: Value, clauses: Pair[], metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    scrutinee: L.listNth(1, v as L.List),
    clauses: L.listToVector(stripSyntax(L.listNth(2, v as L.List)) as L.List).map(asPair),
    metadata
  }
}

export function asQuote (v: Value): { value: Value, metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return { value: L.listNth(1, v as L.List), metadata }
}

export function asSection (v: Value): { values: Value[], metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return { values: L.listToVector(stripSyntax((v as L.List)!.tail) as L.List), metadata }
}

// Statement forms
export const isImport = (v: Value) => isSpecialForm(v, 'import')
export const isDefine = (v: Value) => isSpecialForm(v, 'define')
export const isDisplay = (v: Value) => isSpecialForm(v, 'display')
export const isStruct = (v: Value) => isSpecialForm(v, 'struct')

export function asImport (v: Value): { name: Value, metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return { name: L.listNth(1, v as L.List), metadata }
}

export function asDefine (v: Value): { name: Value, value: Value, metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return { name: L.listNth(1, v as L.List), value: L.listNth(2, v as L.List), metadata }
}

export function asDisplay (v: Value): { value: Value, metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return { value: L.listNth(1, v as L.List), metadata }
}

export function asStruct (v: Value): { name: Value, fields: Value[], metadata: Metadata } {
  const { metadata, value } = unpackSyntax(v)
  v = value
  return {
    name: L.listNth(1, v as L.List),
    fields: L.listToVector(stripSyntax(L.listNth(2, v as L.List)) as L.List),
    metadata
  }
}

export function structPredName (name: string): string {
  return `${name}?`
}

export function structFieldName (name: string, field: string): string {
  return `${name}-${field}`
}