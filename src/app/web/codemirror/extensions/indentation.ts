import {
  indentNodeProp,
  indentRange,
  TreeIndentContext,
} from '@codemirror/language'
import { Extension } from '@codemirror/state'
import { Command, keymap } from '@codemirror/view'
import type { SyntaxNode } from '@lezer/common'
import { INDENT_UNIT, styleOf } from '../../../../scheme/style'

/**
 * DrRacket-style indentation, as a set of Lezer node strategies.
 *
 * A strategy answers exactly one question -- *this line already exists; how far
 * in does it start?* -- for the innermost form enclosing the line. Where the
 * line breaks go is a separate question, answered by the pretty-printer. Both
 * read their rules from `src/scheme/style.ts`, so the two cannot disagree.
 *
 * CodeMirror supplies everything around this: Enter is already bound to
 * `insertNewlineAndIndent`, which asks for the indent with the pending break
 * simulated, and `indentRange` re-indents a whole region, feeding each line's
 * new indent to the next.
 */

/**
 * Whether `node` begins on the opening line of the form being indented -- that
 * is, before the line ends or before the break the caller is simulating.
 * Mirrors CodeMirror's own `bracketedAligned`: pressing Enter directly after
 * `(f` leaves nothing on that line to align against.
 */
function startsOnOpeningLine(cx: TreeIndentContext, node: SyntaxNode): boolean {
  const openLine = cx.state.doc.lineAt(cx.node.from)
  const brk = cx.simulatedBreak
  const end =
    brk === null || brk <= openLine.from
      ? openLine.to
      : Math.min(openLine.to, brk)
  return node.from < end
}

/** Whether the line being indented opens with a closing bracket. */
function isClosingLine(cx: TreeIndentContext): boolean {
  return /^\s*[)\]}]/.test(cx.textAfter)
}

/**
 * The column of this form's own opening bracket.
 *
 * Deliberately not `cx.baseIndent`, which is the indentation of the *line* the
 * form starts on. Those differ whenever a form begins part-way through a line,
 * as in `(f (lambda (x)` -- and there the body belongs two columns past the
 * `(lambda`, not two past the `(f`. Reading the real column is also what keeps
 * this in step with the pretty-printer, which measures the same way.
 */
function formColumn(cx: TreeIndentContext): number {
  return cx.column(cx.node.from)
}

/**
 * Family 2 (rule 7): continuation lines sit under the *first argument*, so
 * `(fun arg1` puts `arg2` at `1 + len("fun") + 1`. `if` is a plain instance of
 * this rule rather than a special case -- its branches land at column 4.
 */
function alignToFirstArg(cx: TreeIndentContext): number {
  if (isClosingLine(cx)) return formColumn(cx)
  const arg = cx.node.firstChild?.nextSibling
  return arg && startsOnOpeningLine(cx, arg)
    ? cx.column(arg.from)
    : formColumn(cx) + INDENT_UNIT
}

/** Family 1 (rules 1, 2, 4, 5): a body indented one unit past the form. */
function alignBody(cx: TreeIndentContext): number {
  return isClosingLine(cx) ? formColumn(cx) : formColumn(cx) + INDENT_UNIT
}

/**
 * A bracketed list -- a binding list, a `cond` clause, a parameter list, a
 * vector -- whose items line up under the first one. With nothing to line up
 * against yet, fall back to one column past the opening bracket, which is where
 * the first item is going to land anyway.
 */
function alignToFirstItem(cx: TreeIndentContext): number {
  if (isClosingLine(cx)) return formColumn(cx)
  const first = cx.node.firstChild
  return first && startsOnOpeningLine(cx, first)
    ? cx.column(first.from)
    : formColumn(cx) + 1
}

/**
 * A parenthesized form, dispatched through the style table: aligned under its
 * first argument, or a body indented one unit.
 *
 * The keyword is the form's first child, since Lezer gives `lambda`, `cond`,
 * and friends their own node types. An application's head is an `Identifier`,
 * which is absent from the table and so takes the default -- `align`, which is
 * exactly rule 7.
 */
function styledForm(cx: TreeIndentContext): number {
  return styleOf(cx.node.firstChild?.name).kind === 'align'
    ? alignToFirstArg(cx)
    : alignBody(cx)
}

/** The indent strategies, for `ScamperLanguage`'s parser props. */
export const scamperIndentation = indentNodeProp.add({
  // Every parenthesized form dispatches through src/scheme/style.ts.
  Application: styledForm,
  Lambda: styledForm,
  If: styledForm,
  And: styledForm,
  Or: styledForm,
  Begin: styledForm,
  Cond: styledForm,
  Let: styledForm,
  Match: styledForm,
  Define: styledForm,
  DefineExport: styledForm,
  Display: styledForm,
  Import: styledForm,
  Export: styledForm,
  Struct: styledForm,
  // A constructor pattern reads like an application: `(cons x` puts `xs` under
  // `x`. Patterns have no entry in the style table, so this is direct.
  PApp: alignToFirstArg,

  // Bracketed lists line their items up under the first one.
  Bindings: alignToFirstItem,
  Binding: alignToFirstItem,
  CondClause: alignToFirstItem,
  MatchClause: alignToFirstItem,
  ArgList: alignToFirstItem,
  FieldList: alignToFirstItem,
  Vector: alignToFirstItem,
  PVector: alignToFirstItem,
  Obj: alignToFirstItem,
})

/**
 * Re-indents every line of the document, leaving the text otherwise untouched.
 * This is DrRacket's Ctrl-I: it fixes indentation without re-flowing anything,
 * so the author's line breaks survive. (Reflowing the document is a different
 * verb -- see `extensions/prettier.ts`.)
 */
export const reindentScamperDocument: Command = (view) => {
  if (view.state.readOnly) return false
  const changes = indentRange(view.state, 0, view.state.doc.length)
  if (!changes.empty) {
    view.dispatch(view.state.update({ changes, userEvent: 'indent' }))
  }
  return true
}

export const IndentationExtension: Extension = keymap.of([
  { key: 'Ctrl-i', run: reindentScamperDocument },
])
