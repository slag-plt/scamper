import { identifierAt, identifierOccurrences } from '../../../../scheme/token'
import { tokenizeAndParse } from '../../../../scheme'
import { makeScopeTreeFromProgram } from '../../../../scheme/scope-tree'
import { resolveBinder } from './scope'
import type { Span } from './definition'

/**
 * Every occurrence of the identifier at [offset] that resolves to the same
 * binding -- respecting scope and shadowing, so a shadowed use elsewhere is
 * excluded. Includes the binder's own site. Works for builtins too (returns
 * their in-file uses).
 * @returns the reference spans (empty when the offset isn't on an identifier)
 */
export async function referencesAt(
  src: string,
  offset: number,
): Promise<Span[]> {
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
  // Every same-spelled token whose own scope resolves to the same binder is a
  // reference; a shadowing binder elsewhere resolves to a different object.
  return identifierOccurrences(src, ident.name)
    .filter((occ) => resolveBinder(tree, ident.name, occ.from) === target)
    .map((occ) => ({ from: occ.from, to: occ.to }))
}
