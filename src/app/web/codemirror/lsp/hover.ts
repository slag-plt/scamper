import type { MarkupContent } from 'vscode-languageserver-protocol'
import { identifierAt } from '../../../../scheme/token'
import { tokenizeAndParse } from '../../../../scheme'
import { docRegistry } from '../../../../lib'
import {
  FunctionDoc,
  parseFunctionDocFromComments,
} from '../../../../scheme/docstring/docstring'
import { functionDocSignature } from '../../../../scheme/docstring/render'

/** The documentation found for an identifier, plus the module it came from (if a builtin). */
interface DocLookup {
  doc: FunctionDoc
  /** The builtin module the binding lives in, or undefined for a user definition. */
  module?: string
}

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
  const found = lookupDoc(src, ident.name)
  if (found === undefined) {
    return null
  }
  return { contents: renderDoc(found), from: ident.from, to: ident.to }
}

/**
 * Resolves an identifier name to its documentation: first the builtin doc
 * registry (robust -- no reparse of the user's buffer), then, as a best
 * effort, a top-level `define` with a docstring in the current source. The
 * user-definition path is skipped silently when the buffer doesn't fully
 * parse, so hovering a builtin still works mid-edit.
 */
function lookupDoc(src: string, name: string): DocLookup | undefined {
  for (const [module, entries] of docRegistry) {
    const doc = entries.get(name)
    if (doc !== undefined) {
      return { doc, module }
    }
  }
  const { program } = tokenizeAndParse(src)
  if (program === undefined) {
    return undefined
  }
  for (const stmt of program) {
    if (
      stmt.tag === 'define' &&
      stmt.name.name === name &&
      stmt.docComments !== undefined
    ) {
      const { doc } = parseFunctionDocFromComments(stmt.docComments)
      if (doc !== undefined) {
        return { doc }
      }
    }
  }
  return undefined
}

/** Renders a doc entry as Markdown: a signature code block, the description, then the source module. */
function renderDoc({ doc, module }: DocLookup): MarkupContent {
  const parts = ['```scheme', functionDocSignature(doc), '```', '', doc.description]
  if (module !== undefined) {
    parts.push('', `_${module}_`)
  }
  return { kind: 'markdown', value: parts.join('\n') }
}
