import type { MarkupContent } from 'vscode-languageserver-protocol'
import { tokenizeAndParse } from '../../../../scheme'
import { parseProgramFromSource } from '../../../../scheme/lezer-bridge'
import { docRegistry } from '../../../../lib'
import * as A from '../../../../scheme/ast'
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
  // A qualified name (`alias.member`) resolves through the module its alias
  // imports, not the flat doc registry.
  if (A.isQualifiedName(name)) {
    return lookupQualifiedDoc(src, name)
  }
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

/**
 * Resolves a qualified name `alias.member` to its documentation: find the
 * import that introduced `alias`, then look up `member` in that module's doc
 * registry. Only built-in modules carry registered docs; a file module has
 * none, so its members resolve to undefined (no hover).
 */
function lookupQualifiedDoc(src: string, name: string): DocLookup | undefined {
  const { qualifier, member } = A.splitQualifiedName(name)
  // Parse tolerantly (not tokenizeAndParse, which yields no program on any
  // error) so hover keeps working while the rest of the buffer is mid-edit --
  // the import statements survive an error elsewhere.
  const program = parseProgramFromSource([], src)
  const imp = A.qualifiedImportMap(program).get(qualifier)
  if (imp === undefined) {
    return undefined
  }
  const doc = docRegistry.get(imp.module)?.get(member)
  return doc !== undefined ? { doc, module: imp.module } : undefined
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
