import type { Layout } from './ast.js'
import {
  DEFAULT_FORMAT_MODE,
  INDENT_UNIT,
  PRINT_WIDTH,
  styleOf,
  type FormatMode,
} from './style.js'
import TextRenderer from '../lpm/renderers/text.js'

/**
 * Line breaking for the surface syntax.
 *
 * This answers the question the editor's indenter does not: *where do the line
 * breaks go?* Both read their rules from {@link styleOf}, so a form laid out
 * here and a form indented in the editor cannot disagree.
 *
 * The output is a {@link LayoutPlan} rather than text, which is what lets the
 * two backends share one decision. `renderToString` turns a plan into a string;
 * LayoutRenderer.vue walks the same tree with the same plan and emits DOM, so
 * value leaves stay components and the trace's changed-path still resolves
 * against the tree it was computed on.
 *
 * The algorithm is the usual one: a group fits on one line or it does not, and
 * when it does not, every child past the opening line goes on a line of its own
 * (never packed greedily). A group's flat width includes its whole subtree, so a
 * form that must break because a subexpression is *long* breaks for free.
 * A subexpression that breaks because a *rule* says so is not visible in any
 * width, though, so that case is propagated explicitly -- see
 * {@link containsForcedBreak}. Both are rule 2b and rule 7's "if any of the
 * subexpressions require multiple lines".
 *
 * Comments are the one thing that breaks a line regardless of width: a line
 * comment runs to the end of its line, so nothing may follow one. A layout only
 * carries comments when it was built from a parse that asked for them (see
 * comments.ts), so a trace pays nothing for any of this.
 */

/** How one group is laid out, once the printer has decided. */
export interface GroupPlan {
  /** Whether the children are spread over several lines. */
  broken: boolean
  /** The column of the group's opening delimiter. */
  col: number
  /** The column at which the group's continuation lines start. */
  indent: number
  /** How many leading children share the group's opening line. */
  onOpeningLine: number
}

/** The decision for every group in a tree, keyed by node identity. */
export type LayoutPlan = Map<Layout, GroupPlan>

const DELIMS = {
  paren: ['(', ')'],
  bracket: ['[', ']'],
  brace: ['{', '}'],
} as const

/** What `node` is laid out from, or nothing when it is a leaf. */
function childrenOf(node: Layout): Layout[] {
  switch (node.kind) {
    case 'group':
    case 'unit':
      return node.children
    case 'hash':
      return [node.child]
    default:
      return []
  }
}

/** Whether `node` or anything beneath it carries a source comment. */
function commented(node: Layout, cache: Map<Layout, boolean>): boolean {
  const hit = cache.get(node)
  if (hit !== undefined) return hit
  const result =
    node.leading !== undefined ||
    node.trailing !== undefined ||
    node.dangling !== undefined ||
    childrenOf(node).some((c) => commented(c, cache))
  cache.set(node, result)
  return result
}

/** What a single pass over a tree needs to hand around. */
interface Ctx {
  /** The page width to lay out for. */
  width: number
  /** How much of the rules' mandated breaking to apply. */
  mode: FormatMode
  widths: Map<Layout, number>
  commented: Map<Layout, boolean>
  forced: Map<Layout, boolean>
  plan: LayoutPlan
}

/**
 * Whether the rules mandate a break here, whatever the width.
 *
 * The *form* breaks under both `strict` and `relaxed` -- rules 1, 3, 4, 5 and 6
 * each draw one shape and offer no alternative -- as does a `let`'s binding
 * list, which rule 4 stacks. A `cond`/`match` clause splits its guard from its
 * consequent only under `strict`, which is rule 5 read to the letter; `relaxed`
 * keeps the two together while they fit, which is how the request's own worked
 * example writes them. `flat` mandates nothing.
 */
function forcedBreak(
  node: Layout & { kind: 'group' },
  mode: FormatMode,
): boolean {
  if (mode === 'flat') return false
  // A group either says so itself -- a clause, a binding list, neither of which
  // the keyword-keyed style table can speak for -- or is a named form.
  const breaks = node.breaks ?? styleOf(node.form).breaks
  return breaks === 'always' || (breaks === 'strict' && mode === 'strict')
}

/**
 * Whether `node`, or anything beneath it, breaks because a rule says so.
 *
 * A form broken by a rule is no wider than one that is not, so -- unlike the
 * 80-column trigger -- an enclosing form cannot infer it from a width. Without
 * this, `(define f (lambda (x) x))` would keep its one-line shape around a
 * lambda that had already split, which is exactly what rule 2b forbids.
 */
function containsForcedBreak(node: Layout, cx: Ctx): boolean {
  const hit = cx.forced.get(node)
  if (hit !== undefined) return hit
  const result =
    (node.kind === 'group' && forcedBreak(node, cx.mode)) ||
    childrenOf(node).some((c) => containsForcedBreak(c, cx))
  cx.forced.set(node, result)
  return result
}

/**
 * Whether `node` must break regardless of its width: either a rule mandates the
 * shape somewhere inside, or a comment runs to the end of its line so the
 * children cannot share one. A comment *after* the group is the enclosing
 * group's problem, not this one's, which is why the node's own `trailing` is not
 * consulted here.
 */
function mustBreak(node: Layout & { kind: 'group' }, cx: Ctx): boolean {
  return (
    containsForcedBreak(node, cx) ||
    node.dangling !== undefined ||
    node.children.some((c) => commented(c, cx.commented))
  )
}

/**
 * Whether a group's children are peers, lining up under the *first* of them,
 * rather than a head and its arguments. Brackets and braces always are; the one
 * parenthesized case is a `let`'s binding list, which says so explicitly.
 */
function alignsUnderItems(node: Layout & { kind: 'group' }): boolean {
  return node.delim !== 'paren' || node.alignItems === true
}

/**
 * The width this node would occupy with no line breaks at all.
 *
 * A `val` is measured as its text rendering even when the DOM backend will draw
 * it as something else (an image, say). That is an approximation, and the right
 * one: a trace's line breaks should not move about depending on which backend
 * is drawing it.
 */
function flatWidth(node: Layout, cache: Map<Layout, number>): number {
  const hit = cache.get(node)
  if (hit !== undefined) return hit
  let width: number
  switch (node.kind) {
    case 'tok':
      width = node.text.length
      break
    case 'val': {
      // A value renders through TextRenderer, which for a form lays it out --
      // so the text may itself span lines. Measure the widest of them: this is
      // the width the value needs, and taking the whole length would count
      // every line as though they ran end to end.
      const text = TextRenderer.render(node.value)
      width = text.split('\n').reduce((m, l) => Math.max(m, l.length), 0)
      break
    }
    case 'hash':
      width = 1 + flatWidth(node.child, cache)
      break
    case 'unit': {
      const inner = node.children.reduce((a, c) => a + flatWidth(c, cache), 0)
      // one space between each pair of children, and no delimiters
      width = inner + Math.max(0, node.children.length - 1)
      break
    }
    case 'group': {
      const inner = node.children.reduce((a, c) => a + flatWidth(c, cache), 0)
      // the two delimiters, plus one space between each pair of children
      width = 2 + inner + Math.max(0, node.children.length - 1)
      break
    }
  }
  cache.set(node, width)
  return width
}

/** How many leading children stay on the opening line of a broken group. */
function openingLineCount(node: Layout & { kind: 'group' }): number {
  const style = styleOf(node.form)
  // A body form holds its keyword and the arguments named by the rule -- the
  // parameter list of a lambda, the name of a define, the scrutinee of a match.
  if (style.kind !== 'align') return 1 + style.head
  // An aligned form keeps its head and first argument together, since the rest
  // line up beneath that argument: "(fun arg1" / "     arg2".
  return alignsUnderItems(node) ? 1 : 2
}

/**
 * Lay out `node` starting at column `col`, recording a plan for every group
 * beneath it.
 *
 * `tail` is how many columns will follow this node on its own last line -- in
 * practice the pile of closing brackets belonging to the forms around it. Lisp
 * ends deeply nested forms with runs like `)))`, so a group that ignored them
 * would sit happily at column 78 and then overflow. Counting them is what makes
 * "eighty columns" mean the finished line.
 *
 * @returns the column just past the node's last character on its final line.
 */
function place(node: Layout, col: number, tail: number, cx: Ctx): number {
  switch (node.kind) {
    case 'tok':
    case 'val':
      return col + flatWidth(node, cx.widths)
    case 'hash':
      return place(node.child, col + 1, tail, cx)
    case 'unit': {
      // A unit is one line by construction, so its children only need the room
      // their siblings and the enclosing tail leave them.
      const kids = node.children
      let cursor = col
      for (let i = 0; i < kids.length; i++) {
        if (i > 0) cursor += 1
        let rest = 0
        for (let j = i + 1; j < kids.length; j++) {
          rest += 1 + flatWidth(kids[j], cx.widths)
        }
        cursor = place(kids[i], cursor, rest + tail, cx)
      }
      return cursor
    }
    case 'group': {
      const kids = node.children
      const broken =
        mustBreak(node, cx) ||
        col + flatWidth(node, cx.widths) + tail > cx.width
      let held = broken
        ? Math.min(openingLineCount(node), kids.length)
        : kids.length
      // A child with comment lines above it has to start a line of its own, so
      // it cannot share the opening one. The first child is the exception: its
      // comments follow the opening delimiter and it keeps its column.
      if (broken) {
        for (let i = 1; i < held; i++) {
          if (kids[i].leading !== undefined) {
            held = i
            break
          }
        }
      }
      // This group's own closing bracket, and whatever follows that. Dangling
      // comments push the bracket onto a line of its own, so it stops counting.
      const closer = 1 + tail
      const trailedByCloser = node.dangling === undefined

      /** Columns following child `i` on the opening line. */
      const afterOnOpeningLine = (i: number): number => {
        let rest = 0
        for (let j = i + 1; j < held; j++)
          rest += 1 + flatWidth(kids[j], cx.widths)
        // When every child is on this line, the closer lands here too;
        // otherwise a line break follows and nothing else does.
        return rest + (held === kids.length && trailedByCloser ? closer : 0)
      }

      // Walk the children sharing the opening line, noting where the last of
      // them starts: for an aligned form, that is where the rest line up.
      let cursor = col + 1
      let alignCol = col + INDENT_UNIT
      for (let i = 0; i < held; i++) {
        if (i > 0) cursor += 1
        if (i === held - 1) alignCol = cursor
        cursor = place(kids[i], cursor, afterOnOpeningLine(i), cx)
      }

      // An aligned form's continuation lines sit under its first argument:
      // where the last child of a full opening line starts, or -- when a
      // comment pushed that argument onto its own line -- where it would have.
      const indent =
        styleOf(node.form).kind !== 'align'
          ? col + INDENT_UNIT
          : held === openingLineCount(node)
            ? alignCol
            : cursor + 1
      cx.plan.set(node, { broken, col, indent, onOpeningLine: held })

      for (let i = held; i < kids.length; i++) {
        // Only the last child shares its line with the closing bracket.
        const rest = i === kids.length - 1 && trailedByCloser ? closer : 0
        cursor = place(kids[i], indent, rest, cx)
      }
      // Dangling comments take the lines after the last child, and the closing
      // bracket one of its own beneath them.
      return trailedByCloser ? cursor + 1 : col + 1
    }
  }
}

/**
 * Decide where every group in `root` breaks, for a page `width` columns wide.
 *
 * @param col the column `root` begins at, when something has already been
 *   written on its first line -- the trace's "--> " marker, say. Every planned
 *   column is absolute, so continuation lines land under the form rather than
 *   under the margin, and `width` still means the finished line.
 */
export function planLayout(
  root: Layout,
  width = PRINT_WIDTH,
  mode: FormatMode = DEFAULT_FORMAT_MODE,
  col = 0,
): LayoutPlan {
  const plan: LayoutPlan = new Map()
  place(root, col, 0, {
    width,
    mode,
    widths: new Map(),
    commented: new Map(),
    forced: new Map(),
    plan,
  })
  return plan
}

/** The separator before child `index` of a planned group: a space or a break. */
export function separatorBefore(
  plan: GroupPlan | undefined,
  index: number,
): { br: true; indent: number } | { br: false } {
  return plan && plan.broken && index >= plan.onOpeningLine
    ? { br: true, indent: plan.indent }
    : { br: false }
}

/**
 * The output under construction.
 *
 * Trailing comments go in `pending` and are written just before the line ends,
 * wherever that turns out to be -- a line comment swallows the rest of its
 * line, so nothing may be printed after one.
 *
 * `indent` is the column the current line owes, held until something is written
 * on it: see {@link write}.
 */
interface Out {
  parts: string[]
  pending: string[]
  indent: number | null
}

/**
 * Write `text` on the current line, indenting it first if nothing is there yet.
 *
 * Indentation is paid this late so that a line nothing is written on stays
 * *empty* -- a blank line between comment paragraphs (#333) would otherwise be
 * padded with spaces, and the editor's indenter empties such a line, so
 * formatting would stop being a fixed point of it (docs/formatting.md).
 */
function write(out: Out, text: string): void {
  if (text === '') return
  if (out.indent !== null) {
    out.parts.push(' '.repeat(out.indent))
    out.indent = null
  }
  out.parts.push(text)
}

/** Hold `comments` back until the current line ends. */
function holdBack(out: Out, comments: string[]): void {
  out.pending.push(...comments)
}

function flushHeld(out: Out): void {
  if (out.pending.length === 0) return
  out.parts.push(' ' + out.pending.join(' '))
  out.pending.length = 0
}

/** End the current line, writing anything held back; the next line owes `col`. */
function endLine(out: Out, col: number): void {
  flushHeld(out)
  out.parts.push('\n')
  out.indent = col
}

/**
 * Write `node`, whose line starts at column `col` -- which only its leading
 * comments need, since those take whole lines above it.
 */
function emit(node: Layout, col: number, plan: LayoutPlan, out: Out): void {
  if (node.leading !== undefined) {
    // A comment cannot share a line with one held back from the line above.
    if (out.pending.length > 0) endLine(out, col)
    for (const c of node.leading) {
      write(out, c)
      endLine(out, col)
    }
  }
  emitBody(node, col, plan, out)
  if (node.trailing !== undefined) holdBack(out, node.trailing)
}

function emitBody(node: Layout, col: number, plan: LayoutPlan, out: Out): void {
  switch (node.kind) {
    case 'tok':
      write(out, node.text)
      return
    case 'val':
      write(out, TextRenderer.render(node.value))
      return
    case 'hash':
      write(out, '#')
      emit(node.child, col + 1, plan, out)
      return
    case 'unit':
      node.children.forEach((child, i) => {
        if (i > 0) write(out, ' ')
        emit(child, col, plan, out)
      })
      return
    case 'group': {
      const [open, close] = DELIMS[node.delim]
      const p = plan.get(node)
      const inner = (p?.col ?? col) + 1
      write(out, open)
      node.children.forEach((child, i) => {
        const sep = separatorBefore(p, i)
        if (i > 0) {
          if (sep.br) endLine(out, sep.indent)
          else write(out, ' ')
        }
        // Only a first child can carry leading comments and still share the
        // opening line; place() puts any other commented child on its own.
        emit(child, sep.br ? sep.indent : inner, plan, out)
      })
      if (node.dangling !== undefined) {
        for (const c of node.dangling) {
          endLine(out, p?.indent ?? inner)
          write(out, c)
        }
        endLine(out, p?.col ?? col)
      }
      write(out, close)
    }
  }
}

/**
 * `root`'s text on one line: what {@link renderToString} gives at infinite
 * width in `flat` mode, reached without the plan where the plan cannot matter.
 *
 * There, every decision {@link planLayout} makes is "no break", so the plan --
 * a width and a forced-break flag per node, an entry per group, and a
 * {@link TextRenderer} pass over every value to measure it -- is built only to
 * be ignored. A trace pays for one of these per step, on a state that grows
 * with the step number (#494), so the plan is worth skipping.
 *
 * A comment is the one thing that breaks a line at any width, so a layout
 * carrying one goes to the planning renderer instead. Both halves are here
 * rather than at the call site: the fast path and the answer it has to match
 * are one decision, and a caller left to write the fallback could write a
 * different one.
 */
export function renderFlat(root: Layout): string {
  const parts: string[] = []
  return flat(root, parts)
    ? parts.join('')
    : renderToString(root, Infinity, 'flat')
}

/** Appends `node`'s flat text to `parts`; false if a comment forbids one line. */
function flat(node: Layout, parts: string[]): boolean {
  if (
    node.leading !== undefined ||
    node.trailing !== undefined ||
    node.dangling !== undefined
  ) {
    return false
  }
  switch (node.kind) {
    case 'tok':
      parts.push(node.text)
      return true
    case 'val':
      parts.push(TextRenderer.render(node.value))
      return true
    case 'hash':
      parts.push('#')
      return flat(node.child, parts)
    case 'unit':
      return flatChildren(node.children, parts)
    case 'group': {
      const [open, close] = DELIMS[node.delim]
      parts.push(open)
      if (!flatChildren(node.children, parts)) return false
      parts.push(close)
      return true
    }
  }
}

/** The children of a unit or group, one space between each pair. */
function flatChildren(children: Layout[], parts: string[]): boolean {
  for (let i = 0; i < children.length; i++) {
    if (i > 0) parts.push(' ')
    if (!flat(children[i], parts)) return false
  }
  return true
}

/**
 * Render `root` to text, breaking lines at `width` columns.
 *
 * @param col the column `root` begins at (see {@link planLayout}). The first
 *   line is *not* indented to it -- whatever put the cursor there has already
 *   written that much -- but every line after it is.
 */
export function renderToString(
  root: Layout,
  width = PRINT_WIDTH,
  mode: FormatMode = DEFAULT_FORMAT_MODE,
  col = 0,
): string {
  const out: Out = { parts: [], pending: [], indent: null }
  emit(root, col, planLayout(root, width, mode, col), out)
  flushHeld(out)
  return out.parts.join('')
}
