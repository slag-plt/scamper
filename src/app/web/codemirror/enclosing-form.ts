import { syntaxTree } from '@codemirror/language'
import type { EditorState } from '@codemirror/state'
import type { SyntaxNode, Tree } from '@lezer/common'

// Lezer node names -> the short labels shown in the status-bar breadcrumb.
// Covers special forms, top-level statements, patterns, and leaf atoms. Node
// types absent from this map (structural wrappers like Program/SExpr, brackets,
// the specialized keyword tokens, and error nodes) are simply skipped when
// building a path -- their meaningful ancestor/child still gets a crumb.
const FORM_LABELS: Record<string, string> = {
  // Statements
  Define: 'define',
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
  Section: 'section',
  Quote: 'quote',
  Apply: 'apply',
  WithHandler: 'with-handler',
  Error: 'error',
  JsVar: 'js-var',
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
    const label = FORM_LABELS[node.name]
    if (label !== undefined) path.push(label)
    node = node.parent
  }
  return path.reverse()
}

/**
 * The enclosing-form breadcrumb at the cursor, read off the editor's live
 * (incrementally-maintained) syntax tree.
 */
export function enclosingFormPath(state: EditorState): string[] {
  return formPathAt(syntaxTree(state), state.selection.main.head)
}
