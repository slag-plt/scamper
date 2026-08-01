import type { MarkupContent } from 'vscode-languageserver-protocol'
import { tokenizeAndParse } from '../../../../scheme'
import { docRegistry } from '../../../../lib'
import {
  FunctionDoc,
  parseFunctionDocFromComments,
} from '../../../../scheme/docstring/docstring'
import { functionDocSignature } from '../../../../scheme/docstring/render'

/** A documentation entry plus the builtin module it came from (undefined for a user definition). */
export interface DocLookup {
  doc: FunctionDoc
  module?: string
}

/** Looks up a name in the builtin doc registry only (no user-source reparse). */
export function findBuiltinDoc(name: string): DocLookup | undefined {
  for (const [module, entries] of docRegistry) {
    const doc = entries.get(name)
    if (doc !== undefined) {
      return { doc, module }
    }
  }
  return undefined
}

/**
 * Resolves an identifier name to its documentation: first the builtin doc
 * registry (robust -- no reparse of the user's buffer), then, as a best
 * effort, a top-level `define` with a docstring in the current source. The
 * user-definition path is skipped silently when the buffer doesn't fully
 * parse, so builtins still resolve mid-edit.
 */
export function lookupFunctionDoc(
  src: string,
  name: string,
): DocLookup | undefined {
  const builtin = findBuiltinDoc(name)
  if (builtin !== undefined) {
    return builtin
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
export function functionDocMarkdown(
  doc: FunctionDoc,
  module?: string,
): MarkupContent {
  const parts = [
    '```scheme',
    functionDocSignature(doc),
    '```',
    '',
    doc.description,
  ]
  if (module !== undefined) {
    parts.push('', `_${module}_`)
  }
  return { kind: 'markdown', value: parts.join('\n') }
}
