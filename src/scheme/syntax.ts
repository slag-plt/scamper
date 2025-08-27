import * as L from '../lpm'

/**
 * A syntax value wraps a value that serves as a Scheme AST. It provides
 * metadata information, e.g., source ranges, for the underlying AST.
 */
export interface Syntax extends L.Struct {
  [L.scamperTag]: 'struct'
  [L.structKind]: 'syntax'
  value: L.Value
  range: L.Range
}

export const mkSyntax = (value: L.Value, range: L.Range = L.Range.none): Syntax =>
  ({ [L.scamperTag]: 'struct', [L.structKind]: 'syntax', value, range })

export const isSyntax = (v: L.Value): v is Syntax => L.isStructKind(v, 'syntax')

/** @return v but with its top-level syntax wrapper removed, if it exists. */
export const stripSyntax = (v: L.Value): L.Value => isSyntax(v) ? v.value : v

/** @return v but with all syntax wrappers removed, recursively. */
export function stripAllSyntax (v: L.Value): L.Value {
  if (isSyntax(v)) {
    return stripAllSyntax(v.value)
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
export const unpackSyntax = (v: L.Value): { value: L.Value, range: L.Range } =>
  isSyntax(v) ?
    { value: v.value, range: v.range } :
    { value: v, range: L.Range.none }
