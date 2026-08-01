import { identifierAt } from '../../../../scheme/token'
import { tokenizeAndParse } from '../../../../scheme'
import { makeScopeTreeFromProgram } from '../../../../scheme/scope-tree'
import { hasSourceRange, resolveBinder } from './scope'

/** A half-open `[from, to)` span in the document. */
export interface Span {
  from: number
  to: number
}

/**
 * Locates the definition of the identifier at [offset]: the binder it resolves
 * to in scope.
 * @returns the binder's span, or null when the offset isn't on a resolvable
 *          identifier or resolves to a builtin/import (which has no in-source
 *          definition to jump to -- those are covered by hover instead)
 */
export async function definitionAt(
  src: string,
  offset: number,
): Promise<Span | null> {
  const ident = identifierAt(src, offset)
  if (ident === undefined) {
    return null
  }
  const { program } = tokenizeAndParse(src)
  if (program === undefined) {
    return null
  }
  const tree = await makeScopeTreeFromProgram(program)
  const binder = resolveBinder(tree, ident.name, offset)
  if (binder === undefined || !hasSourceRange(binder)) {
    return null
  }
  // Scamper ranges are end-inclusive; +1 to the exclusive LSP convention.
  return { from: binder.range.begin.idx, to: binder.range.end.idx + 1 }
}
