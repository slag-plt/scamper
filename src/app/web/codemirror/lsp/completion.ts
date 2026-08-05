import type { CompletionItem } from 'vscode-languageserver-protocol'
import { docRegistry } from '../../../../lib'
import {
  FunctionDoc,
  parseFunctionDocFromComments,
} from '../../../../scheme/docstring/docstring'
import { functionDocSignature } from '../../../../scheme/docstring/render'
import { makeScopeTreeFromProgram } from '../../../../scheme/scope-tree'
import * as A from '../../../../scheme/ast'
import type { Prog } from '../../../../scheme/ast'
import * as SymbolDB from '../../../../scheme/symbol-db'
import { parseProgramFromSource } from '../../../../scheme/lezer-bridge'
import { ScamperDiagnostic } from '../../../../scheme/diagnostic'
import { findBuiltinDoc, functionDocMarkdown } from './docs'
import { rangeAtOffset } from './scope'
import { computeLineStarts, rangeFromOffsets } from './positions'

// CompletionItemKind values (vscode-languageserver-protocol).
const KIND_FUNCTION = 3
const KIND_VARIABLE = 6
const KIND_MODULE = 9

// The run of identifier characters ending at the cursor (its delimiters mirror
// syntax.grammar's Identifier token). Used to detect a qualified `alias.member`
// context mid-word, since a half-typed `alias.` doesn't parse into a program.
const TRAILING_TOKEN = /[^\s()[\]{}'";&]*$/

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
  // Parse tolerantly (not tokenizeAndParse, which yields no program on any
  // error): a half-typed qualified name like `img.` doesn't parse, but the
  // earlier import statements do, and the qualified branch below needs them to
  // resolve the alias mid-edit.
  const diagnostics: ScamperDiagnostic[] = []
  const program = parseProgramFromSource(diagnostics, src)
  // Make sure imported file modules are in the symbol DB before we resolve
  // either qualified members or the flat scope tree (both read from it).
  await SymbolDB.loadTransitiveImports(program)

  // Typing `alias.` (a qualified name) offers that module's members instead of
  // the flat scope -- nothing else is in scope after the dot.
  const qualified = qualifiedMemberCompletions(src, offset, program)
  if (qualified !== undefined) {
    return qualified
  }

  // Outside a qualified context, a buffer that doesn't parse cleanly falls back
  // to prelude so completion still works mid-edit.
  if (diagnostics.length > 0) {
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
  // Surface qualified-import aliases so the user can discover `alias.member`.
  for (const alias of A.qualifiedImportMap(program).keys()) {
    if (!seen.has(alias)) {
      seen.add(alias)
      items.push({ label: alias, kind: KIND_MODULE })
    }
  }
  return items
}

/**
 * When the cursor sits in a qualified name `alias.partial` for a known
 * qualified-import alias, the completion candidates are that module's exported
 * members (as `alias.member`), each with a textEdit spanning the whole dotted
 * token so insertion is correct regardless of the editor's word boundaries
 * (`.` isn't a word character). Returns undefined when not in a qualified
 * context, so the caller falls back to ordinary scope completion.
 */
function qualifiedMemberCompletions(
  src: string,
  offset: number,
  program: Prog,
): CompletionItem[] | undefined {
  const token = TRAILING_TOKEN.exec(src.slice(0, offset))?.[0] ?? ''
  if (!A.isQualifiedName(token)) {
    return undefined
  }
  const { qualifier } = A.splitQualifiedName(token)
  const imp = A.qualifiedImportMap(program).get(qualifier)
  if (imp === undefined) {
    return undefined
  }
  const range = rangeFromOffsets(offset - token.length, offset, computeLineStarts(src))
  const docs = docRegistry.get(imp.module)
  const items: CompletionItem[] = []
  const seen = new Set<string>()
  for (const { name: member } of SymbolDB.get(imp.module) ?? []) {
    if (member.includes('##') || seen.has(member)) {
      continue
    }
    seen.add(member)
    const label = `${qualifier}.${member}`
    const doc = docs?.get(member)
    items.push({
      label,
      kind: doc !== undefined ? KIND_FUNCTION : KIND_VARIABLE,
      filterText: label,
      textEdit: { range, newText: label },
      ...(doc !== undefined
        ? {
            detail: functionDocSignature(doc).split('\n')[0],
            documentation: functionDocMarkdown(doc, imp.module),
          }
        : {}),
    })
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
