import type { Layout } from './ast.js'
import { INDENT_UNIT, PRINT_WIDTH, styleOf } from './style.js'
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
 * (never packed greedily). Because a group's flat width includes its whole
 * subtree, a form that has to break because a subexpression is long breaks for
 * free -- no separate "contains a hard break" propagation is needed.
 */

/** How one group is laid out, once the printer has decided. */
export interface GroupPlan {
  /** Whether the children are spread over several lines. */
  broken: boolean
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
    case 'val':
      width = TextRenderer.render(node.value).length
      break
    case 'hash':
      width = 1 + flatWidth(node.child, cache)
      break
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
function place(
  node: Layout,
  col: number,
  tail: number,
  width: number,
  widths: Map<Layout, number>,
  plan: LayoutPlan,
): number {
  switch (node.kind) {
    case 'tok':
    case 'val':
      return col + flatWidth(node, widths)
    case 'hash':
      return place(node.child, col + 1, tail, width, widths, plan)
    case 'group': {
      const kids = node.children
      const broken = col + flatWidth(node, widths) + tail > width
      const held = broken
        ? Math.min(openingLineCount(node), kids.length)
        : kids.length
      // This group's own closing bracket, and whatever follows that.
      const closer = 1 + tail

      /** Columns following child `i` on the opening line. */
      const afterOnOpeningLine = (i: number): number => {
        let rest = 0
        for (let j = i + 1; j < held; j++)
          rest += 1 + flatWidth(kids[j], widths)
        // When every child is on this line, the closer lands here too;
        // otherwise a line break follows and nothing else does.
        return rest + (held === kids.length ? closer : 0)
      }

      // Walk the children sharing the opening line, noting where the last of
      // them starts: for an aligned form, that is where the rest line up.
      let cursor = col + 1
      let alignCol = col + INDENT_UNIT
      for (let i = 0; i < held; i++) {
        if (i > 0) cursor += 1
        if (i === held - 1) alignCol = cursor
        cursor = place(
          kids[i],
          cursor,
          afterOnOpeningLine(i),
          width,
          widths,
          plan,
        )
      }

      const indent =
        styleOf(node.form).kind === 'align' && held === openingLineCount(node)
          ? alignCol
          : col + INDENT_UNIT
      plan.set(node, { broken, indent, onOpeningLine: held })

      for (let i = held; i < kids.length; i++) {
        // Only the last child shares its line with the closing bracket.
        const rest = i === kids.length - 1 ? closer : 0
        cursor = place(kids[i], indent, rest, width, widths, plan)
      }
      return cursor + 1
    }
  }
}

/** Decide where every group in `root` breaks, for a page `width` columns wide. */
export function planLayout(root: Layout, width = PRINT_WIDTH): LayoutPlan {
  const plan: LayoutPlan = new Map()
  place(root, 0, 0, width, new Map(), plan)
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

function emit(node: Layout, plan: LayoutPlan, out: string[]): void {
  switch (node.kind) {
    case 'tok':
      out.push(node.text)
      return
    case 'val':
      out.push(TextRenderer.render(node.value))
      return
    case 'hash':
      out.push('#')
      emit(node.child, plan, out)
      return
    case 'group': {
      const [open, close] = DELIMS[node.delim]
      const p = plan.get(node)
      out.push(open)
      node.children.forEach((child, i) => {
        if (i > 0) {
          const sep = separatorBefore(p, i)
          out.push(sep.br ? `\n${' '.repeat(sep.indent)}` : ' ')
        }
        emit(child, plan, out)
      })
      out.push(close)
    }
  }
}

/** Render `root` to text, breaking lines at `width` columns. */
export function renderToString(root: Layout, width = PRINT_WIDTH): string {
  const out: string[] = []
  emit(root, planLayout(root, width), out)
  return out.join('')
}
