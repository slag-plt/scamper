import { Layout, layoutToFlatString } from './ast'

/**
 * Locating the one sub-expression that changed between two steps of a trace.
 *
 * A reduction rewrites exactly one redex and leaves the rest of the expression
 * alone, so the two steps agree everywhere but one subtree. Walking down while
 * exactly one child differs finds it; where two or more differ, the change is
 * wider than any single child and the node holding them is the answer.
 *
 * The diff is over `Layout` rather than `Exp` because Layout is what actually
 * gets drawn -- an application becomes a parenthesized group, `#(...)` gains
 * its hash -- so a path through it lands on the same node the renderer is
 * about to produce. A path through the AST would still have to be translated.
 */

/** The children a layout renders, or null for a leaf. */
function childrenOf(l: Layout): Layout[] | null {
  switch (l.kind) {
    case 'group':
    case 'unit':
      return l.children
    case 'hash':
      return [l.child]
    default:
      return null
  }
}

/** Whether two layouts draw the same thing. */
function same(a: Layout, b: Layout): boolean {
  return layoutToFlatString(a) === layoutToFlatString(b)
}

/**
 * @returns the path from `after`'s root to the sub-layout that differs from
 *   `before` -- child indices to follow, `[]` for the root itself -- or null
 *   when the two are identical.
 */
export function changedLayoutPath(
  before: Layout,
  after: Layout,
): number[] | null {
  if (same(before, after)) return null

  const a = childrenOf(before)
  const b = childrenOf(after)
  // A leaf, a change of shape, or a group that gained or lost children: the
  // difference is this node, with nothing finer to point at.
  if (before.kind !== after.kind || a === null || a.length !== b?.length) {
    return []
  }
  if (before.kind === 'group' && after.kind === 'group' && before.delim !== after.delim) {
    return []
  }

  const differing = a.flatMap((child, i) => (same(child, b[i]) ? [] : [i]))
  // Two or more children moved, so no one of them is "the" change. (Zero is
  // unreachable -- the layouts differ -- but falls here safely.)
  if (differing.length !== 1) return []

  const i = differing[0]
  const inner = changedLayoutPath(a[i], b[i])
  return inner === null ? [i] : [i, ...inner]
}
