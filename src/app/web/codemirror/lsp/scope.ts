import * as A from '../../../../scheme/ast'
import { Loc, Range } from '../../../../lpm'
import { ScopeTree } from '../../../../scheme/scope-tree'

/** A zero-width range at [offset], for scope containment queries (compared by idx). */
export function rangeAtOffset(offset: number): Range {
  const loc = new Loc(0, 0, offset)
  return new Range(loc, loc)
}

/**
 * The binder that [name] resolves to at [offset], or undefined if it isn't
 * bound there. Visible identifiers are ordered innermost-first, so the first
 * match is the one in scope (respecting shadowing).
 */
export function resolveBinder(
  tree: ScopeTree,
  name: string,
  offset: number,
): A.Identifier | undefined {
  const scope = tree.getInnermostScope(rangeAtOffset(offset)) ?? tree
  return scope.getVisibleIdentifiers().find((id) => id.name === name)
}

/** Whether a binder has a real source location (builtins/imports carry Range.none). */
export function hasSourceRange(binder: A.Identifier): boolean {
  return binder.range.begin.idx >= 0
}
