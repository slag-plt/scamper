import { expToString } from '../ast'
import { FunctionDoc, Pred } from './docstring'
import { isCategoryTag } from './tags/category-tag'

/** The documented name of a function/constant, e.g. `"caddr"`. */
export function functionDocName(doc: FunctionDoc): string {
  return doc.signature.function.head.name
}

/** Flattens every `@category` tag's contents into one list, e.g. `["list", "predicates"]`. */
export function functionDocCategories(doc: FunctionDoc): string[] {
  return doc.tags.filter(isCategoryTag).flatMap((t) => t.contents)
}

/** Renders a predicate as source-like text, e.g. `boolean?` or `(list-of number?)`. */
export function predToString(pred: Pred): string {
  return expToString(pred)
}

/**
 * A predicate's bare "type name" for filtering/matching purposes, e.g.
 * `boolean?` -> `boolean`, `(list-of number?)` -> `list-of`.
 */
export function predTypeName(pred: Pred): string {
  const name = pred.tag === 'id' ? pred.name : pred.head.name
  return name.endsWith('?') ? name.slice(0, -1) : name
}

/**
 * Renders a full signature block matching the old hand-written `Doc`
 * viewers' format, e.g.:
 *   (cons v1 v2) -> pair?
 *     v1: any
 *     v2: any
 * or, for a documented constant (no params, no rest param):
 *   pi : number?
 */
export function functionDocSignature(doc: FunctionDoc): string {
  const name = functionDocName(doc)
  const returnStr = predToString(doc.signature.predicate)
  if (doc.params.length === 0 && !doc.restParam) {
    return `${name} : ${returnStr}`
  }
  const argNames = [
    ...doc.params.map((p) => p.name),
    ...(doc.restParam ? [`. ${doc.restParam.name}`] : []),
  ].join(' ')
  const paramLine = (p: FunctionDoc['params'][number]) =>
    `  ${p.name}: ${predToString(p.predicate)}${p.description ? `\n    ${p.description}` : ''}`
  const argLines = [
    ...doc.params.map(paramLine),
    ...(doc.restParam ? [paramLine(doc.restParam)] : []),
  ].join('\n')
  return `(${name} ${argNames}) -> ${returnStr}\n${argLines}`
}
