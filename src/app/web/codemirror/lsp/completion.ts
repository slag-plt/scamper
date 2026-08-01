import type { CompletionItem } from 'vscode-languageserver-protocol'
import { tokenizeAndParse } from '../../../../scheme'
import { docRegistry } from '../../../../lib'
import {
  FunctionDoc,
  parseFunctionDocFromComments,
} from '../../../../scheme/docstring/docstring'
import { functionDocSignature } from '../../../../scheme/docstring/render'
import { makeScopeTreeFromProgram } from '../../../../scheme/scope-tree'
import type { Prog } from '../../../../scheme/ast'
import { findBuiltinDoc, functionDocMarkdown } from './docs'
import { rangeAtOffset } from './scope'

// CompletionItemKind values (vscode-languageserver-protocol).
const KIND_FUNCTION = 3
const KIND_VARIABLE = 6

/**
 * Completion candidates in scope at [offset]: everything visible per the scope
 * tree -- builtins from prelude/imported modules, the program's own top-level
 * definitions, and locals (lambda/let/match bindings) enclosing the cursor.
 * Inner bindings shadow outer ones. Documented names carry their signature and
 * docs. On a buffer that doesn't parse, falls back to prelude so completion
 * still works mid-edit. The editor filters this list by the typed prefix.
 */
export async function completionsFor(
  src: string,
  offset: number,
): Promise<CompletionItem[]> {
  const { program } = tokenizeAndParse(src)
  if (program === undefined) {
    return preludeFallback()
  }
  const userDocs = topLevelUserDocs(program)
  const tree = await makeScopeTreeFromProgram(program)
  const scope = tree.getInnermostScope(rangeAtOffset(offset)) ?? tree

  const items: CompletionItem[] = []
  const seen = new Set<string>()
  for (const id of scope.getVisibleIdentifiers()) {
    // `##...##` names are internal machinery; the first (innermost) binding of
    // a name wins, so later duplicates are dropped.
    if (id.name.includes('##') || seen.has(id.name)) {
      continue
    }
    seen.add(id.name)
    items.push(itemFor(id.name, userDocs))
  }
  return items
}

/** A completion item for a name, documented from a builtin or a user docstring where available. */
function itemFor(
  name: string,
  userDocs: Map<string, FunctionDoc>,
): CompletionItem {
  const builtin = findBuiltinDoc(name)
  if (builtin !== undefined) {
    return documentedItem(name, builtin.doc, builtin.module)
  }
  const userDoc = userDocs.get(name)
  if (userDoc !== undefined) {
    return documentedItem(name, userDoc)
  }
  return { label: name, kind: KIND_VARIABLE }
}

function documentedItem(
  name: string,
  doc: FunctionDoc,
  module?: string,
): CompletionItem {
  return {
    label: name,
    kind: KIND_FUNCTION,
    detail: functionDocSignature(doc).split('\n')[0],
    documentation: functionDocMarkdown(doc, module),
  }
}

/** name -> FunctionDoc for the program's documented top-level definitions. */
function topLevelUserDocs(program: Prog): Map<string, FunctionDoc> {
  const docs = new Map<string, FunctionDoc>()
  for (const stmt of program) {
    if (stmt.tag === 'define' && stmt.docComments !== undefined) {
      const { doc } = parseFunctionDocFromComments(stmt.docComments)
      if (doc !== undefined) {
        docs.set(stmt.name.name, doc)
      }
    }
  }
  return docs
}

/** Documented prelude bindings, used when the buffer doesn't parse into a scope tree. */
function preludeFallback(): CompletionItem[] {
  const entries = docRegistry.get('prelude')
  if (entries === undefined) {
    return []
  }
  return [...entries].map(([name, doc]) => documentedItem(name, doc, 'prelude'))
}
