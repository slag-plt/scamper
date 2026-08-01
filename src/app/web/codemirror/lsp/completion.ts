import type { CompletionItem } from 'vscode-languageserver-protocol'
import { tokenizeAndParse } from '../../../../scheme'
import { docRegistry } from '../../../../lib'
import { parseFunctionDocFromComments } from '../../../../scheme/docstring/docstring'
import { functionDocSignature } from '../../../../scheme/docstring/render'
import type { Define } from '../../../../scheme/ast'
import { functionDocMarkdown } from './docs'

// CompletionItemKind values (vscode-languageserver-protocol).
const KIND_FUNCTION = 3
const KIND_VARIABLE = 6
const KIND_CONSTRUCTOR = 4

/**
 * Completion candidates visible at the top level of [src]: the always-imported
 * `prelude`, any builtin modules the program imports, and the program's own
 * top-level `define`/`struct` names. Locals (lambda/let bindings) are not yet
 * included -- that needs scope-tree wiring. The editor filters this list by the
 * typed prefix, so it's returned whole.
 */
export function completionsFor(src: string): CompletionItem[] {
  const items = new Map<string, CompletionItem>()
  addModule(items, 'prelude')

  const { program } = tokenizeAndParse(src)
  if (program !== undefined) {
    for (const stmt of program) {
      if (stmt.tag === 'import' && stmt.kind === 'builtin') {
        addModule(items, stmt.module)
      } else if (stmt.tag === 'define') {
        // A user definition shadows a builtin of the same name.
        items.set(stmt.name.name, defineItem(stmt))
      } else if (stmt.tag === 'struct') {
        items.set(stmt.name.name, {
          label: stmt.name.name,
          kind: KIND_CONSTRUCTOR,
          detail: 'struct',
        })
      }
    }
  }
  return [...items.values()]
}

/** Adds every documented binding of a builtin module to the candidate map. */
function addModule(items: Map<string, CompletionItem>, module: string): void {
  const entries = docRegistry.get(module)
  if (entries === undefined) {
    return
  }
  for (const [name, doc] of entries) {
    items.set(name, {
      label: name,
      kind: KIND_FUNCTION,
      detail: functionDocSignature(doc).split('\n')[0],
      documentation: functionDocMarkdown(doc, module),
    })
  }
}

/** Builds a completion item for a top-level user definition, with its docstring if present. */
function defineItem(stmt: Define): CompletionItem {
  const item: CompletionItem = {
    label: stmt.name.name,
    kind: stmt.value.tag === 'lam' ? KIND_FUNCTION : KIND_VARIABLE,
  }
  if (stmt.docComments !== undefined) {
    const { doc } = parseFunctionDocFromComments(stmt.docComments)
    if (doc !== undefined) {
      item.detail = functionDocSignature(doc).split('\n')[0]
      item.documentation = functionDocMarkdown(doc)
    }
  }
  return item
}
