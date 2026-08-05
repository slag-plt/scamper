import { syntaxTree } from '@codemirror/language'
import type { EditorState, Text } from '@codemirror/state'
import type { SyntaxNode, Tree } from '@lezer/common'

// Lezer node names -> the short labels shown in the status-bar breadcrumb.
// Covers special forms, top-level statements, patterns, and leaf atoms. Node
// types absent from this map (structural wrappers like Program/SExpr, brackets,
// the specialized keyword tokens, and error nodes) are simply skipped when
// building a path -- their meaningful ancestor/child still gets a crumb.
const FORM_LABELS: Record<string, string> = {
  // Statements
  Define: 'define',
  DefineExport: 'define-export',
  Export: 'export',
  Display: 'display',
  Struct: 'struct',
  Import: 'import',
  // Special forms
  Lambda: 'lambda',
  If: 'if',
  Let: 'let',
  LetStar: 'let*',
  Cond: 'cond',
  Match: 'match',
  And: 'and',
  Or: 'or',
  Begin: 'begin',
  AnonFn: 'anonymous function',
  Quote: 'quote',
  Vector: 'vector',
  Application: 'application',
  // Patterns
  PApp: 'pattern',
  PVector: 'vector pattern',
  // Atoms
  Number: 'number',
  String: 'string',
  Boolean: 'boolean',
  Char: 'char',
  Identifier: 'identifier',
  LineComment: 'comment',
}

/**
 * Builds the breadcrumb of enclosing syntactic forms at a document offset.
 * @param tree a Lezer parse tree of a Scamper program
 * @param pos a document offset
 * @returns short labels, outermost statement first down to the leaf at `pos`
 */
export function formPathAt(tree: Tree, pos: number): string[] {
  const path: string[] = []
  let node: SyntaxNode | null = tree.resolveInner(pos, -1)
  while (node) {
    // Record indexing is typed as `string`, but this is a partial map: node
    // types with no label (wrappers, brackets, keyword tokens) return undefined.
    const label = FORM_LABELS[node.name] as string | undefined
    if (label !== undefined) path.push(label)
    node = node.parent
  }
  return path.reverse()
}

/**
 * The 1-based line and 0-based column offset of `offset` within `doc`. The
 * 0-based column matches Loc's convention; callers that display it (the status
 * bar) add 1.
 */
export function lineColumnAt(
  doc: Text,
  offset: number,
): { line: number; columnOffset: number } {
  const line = doc.lineAt(offset)
  return { line: line.number, columnOffset: offset - line.from }
}

/** The cursor's position and enclosing-form breadcrumb, for the status bar. */
export interface CursorStatus {
  /** 1-based line number. */
  line: number
  /** 1-based column number. */
  column: number
  /** Enclosing-form breadcrumb, outermost first (see {@link formPathAt}). */
  path: string[]
}

/**
 * Reads the cursor's line/column and enclosing-form breadcrumb off the editor's
 * live (incrementally-maintained) syntax tree.
 */
export function cursorStatus(state: EditorState): CursorStatus {
  const head = state.selection.main.head
  const { line, columnOffset } = lineColumnAt(state.doc, head)
  return {
    line,
    column: columnOffset + 1,
    path: formPathAt(syntaxTree(state), head),
  }
}

/**
 * Wraps `notify` so it fires only when the cursor status actually changes -- a
 * cheap guard against redundant notifications (e.g. an edit that leaves the
 * cursor put). Each call returns a fresh deduper with its own memory.
 */
export function dedupeCursorStatus(
  notify: (status: CursorStatus) => void,
): (status: CursorStatus) => void {
  let lastKey: string | null = null
  return (status) => {
    const key = [status.line, status.column, ...status.path].join('|')
    if (key !== lastKey) {
      lastKey = key
      notify(status)
    }
  }
}
