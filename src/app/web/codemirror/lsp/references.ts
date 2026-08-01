import { documentHighlightsAt } from './highlight'
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
  return (await documentHighlightsAt(src, offset)).map(({ from, to }) => ({
    from,
    to,
  }))
}
