import { identifierAt, identifierOccurrences } from '../../../../scheme/token'
import { tokenizeAndParse } from '../../../../scheme'
import { makeScopeTreeFromProgram } from '../../../../scheme/scope-tree'
import { hasSourceRange, resolveBinder } from './scope'
import type { Span } from './definition'

/** An occurrence of an identifier, flagged as the binding site (`write`) or a use (`read`). */
export interface Highlight extends Span {
  write: boolean
}

/**
 * Every occurrence of the identifier at [offset] that resolves to the same
 * binding, with the binder's own site flagged `write`. Respects scope and
 * shadowing. Builtins/imports have no in-buffer binder, so all their
 * occurrences are `read`.
 */
export async function documentHighlightsAt(
  src: string,
  offset: number,
): Promise<Highlight[]> {
  const ident = identifierAt(src, offset)
  if (ident === undefined) {
    return []
  }
  const { program } = tokenizeAndParse(src)
  if (program === undefined) {
    return []
  }
  const tree = await makeScopeTreeFromProgram(program)
  const target = resolveBinder(tree, ident.name, offset)
  if (target === undefined) {
    return []
  }
  const binderFrom = hasSourceRange(target) ? target.range.begin.idx : -1
  return identifierOccurrences(src, ident.name)
    .filter((occ) => resolveBinder(tree, ident.name, occ.from) === target)
    .map((occ) => ({ from: occ.from, to: occ.to, write: occ.from === binderFrom }))
}
