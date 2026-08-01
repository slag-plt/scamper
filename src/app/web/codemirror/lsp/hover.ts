import type { MarkupContent } from 'vscode-languageserver-protocol'
import { identifierAt } from '../../../../scheme/token'
import { functionDocMarkdown, lookupFunctionDoc } from './docs'

/** Hover content plus the half-open `[from, to)` span of the identifier it describes. */
export interface HoverResult {
  contents: MarkupContent
  from: number
  to: number
}

/**
 * Builds hover documentation for the identifier at [offset], if any.
 * @returns the rendered docs and the identifier's span, or null when the
 *          offset isn't on a documented identifier
 */
export function hoverAt(src: string, offset: number): HoverResult | null {
  const ident = identifierAt(src, offset)
  if (ident === undefined) {
    return null
  }
  const found = lookupFunctionDoc(src, ident.name)
  if (found === undefined) {
    return null
  }
  return {
    contents: functionDocMarkdown(found.doc, found.module),
    from: ident.from,
    to: ident.to,
  }
}
