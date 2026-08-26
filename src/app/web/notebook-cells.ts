/**
 * How a Scamper file reads as a notebook (#410).
 *
 * A notebook is the file split into cells: one per top-level form, with the
 * comments between them as prose. Nothing is stored in the file to say where
 * the cells are -- the parser already knows, since a Scamper program *is* a
 * list of statements, and that mapping is the whole of this module.
 *
 * Pure data and pure functions over it, as panel-layout.ts is, so the rules
 * here can be unit-tested without an editor, a document, or a DOM.
 */
import type { Comment } from '../../scheme/ast'
import { collectComments } from '../../scheme/comments'
import { tokenizeAndParse } from '../../scheme'

/**
 * One cell: a stretch of the file, and how to show it.
 *
 * Offsets are into the source it was split from, half-open as CodeMirror's
 * are. Cells never overlap and are in source order, but they do not cover the
 * file: the whitespace between them belongs to nobody, which is what lets the
 * notebook leave a file's spacing exactly as its author left it.
 */
export type Cell = CodeCell | ProseCell

interface CellBase {
  from: number
  to: number
  /** The text the cell owns: `src.slice(from, to)`. */
  text: string
}

/** A top-level form, with any docstring written above it. */
export interface CodeCell extends CellBase {
  kind: 'code'
  /**
   * The form itself, without the docstring above it or the comment beside it.
   *
   * `src.slice(stmtFrom, stmtTo).trim()` is exactly what the scheduler passes
   * to `beginStatement` for this form, which is what lets a run's output be
   * matched back to the cell that produced it (see notebook-display.ts).
   */
  stmtFrom: number
  stmtTo: number
}

/** The text a run's `beginStatement` will announce for `cell`. */
export function captionOf(cell: CodeCell, src: string): string {
  return src.slice(cell.stmtFrom, cell.stmtTo).trim()
}

/** A run of ordinary comments between two forms, shown as Markdown. */
export interface ProseCell extends CellBase {
  kind: 'prose'
}

/**
 * Splits `src` into cells.
 *
 * @returns the cells, or null if `src` does not parse -- a notebook mid-edit
 *          usually does not, and the caller is expected to keep the split it
 *          already has rather than take cells away while they are being typed.
 */
export function splitIntoCells(src: string): Cell[] | null {
  const { program } = tokenizeAndParse(src)
  if (program === undefined) return null

  const stmts = program.map((s) => ({
    from: s.range.begin.idx,
    to: s.range.end.idx + 1,
    line: s.range.begin.line,
  }))
  // Comments inside a form -- one on a `cond` clause, say -- are part of that
  // form's text and are not cells of their own.
  const free = collectComments(src).filter(
    (c) =>
      !stmts.some(
        (s) => c.range.begin.idx >= s.from && c.range.end.idx < s.to,
      ),
  )

  const cells: Cell[] = []
  let next = 0
  for (const stmt of stmts) {
    const pending: Comment[] = []
    while (next < free.length && free[next].range.begin.idx < stmt.from) {
      pending.push(free[next++])
    }
    absorbTrailing(cells, pending, src)
    // A docstring is documentation for the form below it, so it stays in that
    // form's cell rather than becoming prose: it is what the docs, the
    // contracts and the `@example` checks read, and a student editing it is
    // editing the function.
    const doc = docBlockFor(pending, stmt.line)
    for (const run of runs(pending.slice(0, pending.length - doc.length))) {
      cells.push(proseCell(run, src))
    }
    const from = doc.length > 0 ? doc[0].range.begin.idx : stmt.from
    cells.push({
      kind: 'code',
      from,
      to: stmt.to,
      text: src.slice(from, stmt.to),
      stmtFrom: stmt.from,
      stmtTo: stmt.to,
    })
  }

  // Whatever is below the last form: a closing comment, or the whole of a file
  // that is nothing but comments.
  const rest = free.slice(next)
  absorbTrailing(cells, rest, src)
  for (const run of runs(rest)) cells.push(proseCell(run, src))
  return cells
}

/**
 * Moves a comment written *beside* the last cell into it, if `pending` starts
 * with one.
 *
 * `(define x 5) ; five` is one line of the student's file and reads as one, so
 * it is one cell rather than a form with a paragraph of prose stuck to it.
 */
function absorbTrailing(cells: Cell[], pending: Comment[], src: string): void {
  const last = cells.at(-1)
  if (last === undefined || pending.length === 0) return
  const lastLine = lineOf(src, last.to - 1)
  if (pending[0].range.begin.line !== lastLine) return
  const comment = pending.shift()
  // Unreachable -- the length was checked above -- but shift's type says so.
  if (comment === undefined) return
  last.to = comment.range.end.idx + 1
  last.text = src.slice(last.from, last.to)
}

/** @returns the 1-based line `idx` falls on. */
function lineOf(src: string, idx: number): number {
  let line = 1
  for (let i = 0; i < idx && i < src.length; i++) {
    if (src[i] === '\n') line++
  }
  return line
}

/** Whether a comment is a docstring line rather than an ordinary one. */
function isDocLine(c: Comment): boolean {
  return c.line.trimStart().startsWith(';;;')
}

/**
 * @returns the comments at the end of `pending` that are the docstring of the
 *          form on `statementLine`, and so belong in its cell.
 *
 * The rule is the compiler's, so that the two cannot disagree about what
 * documents what (#413): the block directly above a form is the one with no
 * blank line between it and the form, and a contiguous block is one block --
 * an ordinary comment line between two `;;;` lines does not split it, because
 * nothing in the source says it should. A blank line is how a block is set
 * apart, and then it is a header rather than a docstring, and the notebook
 * shows it as the prose it reads as.
 */
function docBlockFor(pending: Comment[], statementLine: number): Comment[] {
  let i = pending.length
  let wanted = statementLine - 1
  while (i > 0 && pending[i - 1].range.begin.line === wanted) {
    wanted--
    i--
  }
  const block = pending.slice(i)
  // Prose, if nothing in it is documentation.
  return block.some(isDocLine) ? block : []
}

/**
 * Groups comments into paragraphs, breaking at a blank line.
 *
 * A blank line is how someone separates one remark from the next, so it is
 * also how they separate one cell from the next.
 */
function runs(comments: Comment[]): Comment[][] {
  const out: Comment[][] = []
  for (const c of comments) {
    const current = out.at(-1)
    if (
      current === undefined ||
      c.range.begin.line - (current.at(-1)?.range.end.line ?? 0) > 1
    ) {
      out.push([c])
    } else {
      current.push(c)
    }
  }
  return out
}

function proseCell(run: Comment[], src: string): ProseCell {
  const from = run[0].range.begin.idx
  const to = run[run.length - 1].range.end.idx + 1
  return { kind: 'prose', from, to, text: src.slice(from, to) }
}

/**
 * The Markdown a prose cell holds: its comment lines with their `;` markers
 * taken off, which is both what is rendered and what is edited.
 */
export function proseToMarkdown(text: string): string {
  return text
    .split('\n')
    .map((line) => line.replace(/^\s*;+ ?/, ''))
    .join('\n')
}

/**
 * The inverse: Markdown written back as comment lines.
 *
 * Nothing written is nothing to write: a text cell left empty puts no bare `;`
 * into the file, so opening one and changing your mind costs nothing.
 *
 * @param indent the whitespace the cell started at, so a comment nested inside
 *        a file's indentation stays where it was.
 */
export function markdownToProse(markdown: string, indent = ''): string {
  if (markdown.trim().length === 0) return ''
  const lines = markdown.split('\n')
  // A blank line at the end says nothing in Markdown, and would be a bare `;`
  // in the file: one for every time someone pressed Enter and thought better
  // of it, and one more each time a multi-line paste left a line behind.
  while (lines.length > 0 && lines[lines.length - 1].trim().length === 0) {
    lines.pop()
  }
  return lines
    .map((line) => (line.length === 0 ? `${indent};` : `${indent}; ${line}`))
    .join('\n')
}

/**
 * The cells after an edit of `[from, to)` that inserted `insertedLength`
 * characters, for a notebook that need not re-split.
 *
 * Editing a cell moves every cell below it, and re-parsing to discover by how
 * much would be both slower and unavailable: a file is unparseable for most of
 * the time someone is typing in it. The cell that was edited keeps its own
 * text, which the caller has, so only the offsets move.
 *
 * @returns the shifted cells, or null if the edit crossed a cell boundary --
 *          a paste over several cells, an undo -- which only a re-split can
 *          make sense of.
 */
export function shiftCells<C extends Cell>(
  cells: C[],
  from: number,
  to: number,
  insertedLength: number,
  src: string,
): C[] | null {
  const edited = cells.findIndex((c) => from >= c.from && to <= c.to)
  if (edited === -1) return null
  const delta = insertedLength - (to - from)

  // Where a position inside the edited cell goes. A start stays put when
  // something is typed at it and an end moves, so the cell *grows* around what
  // was typed at its edge rather than leaving it outside -- which is the whole
  // of what an empty cell being typed into for the first time is.
  const start = (pos: number) =>
    pos <= from ? pos : pos >= to ? pos + delta : from
  const end = (pos: number) =>
    pos < from ? pos : pos >= to ? pos + delta : from + insertedLength

  return cells.map((cell, i) => {
    // Cells are in source order and do not overlap, so one above the edit is
    // wholly above it and one below is wholly below.
    const moved: C =
      i < edited
        ? { ...cell }
        : i > edited
          ? { ...cell, from: cell.from + delta, to: cell.to + delta }
          : { ...cell, from: start(cell.from), to: end(cell.to) }
    moved.text = src.slice(moved.from, moved.to)
    if (moved.kind === 'code' && i >= edited) {
      moved.stmtFrom = i > edited ? moved.stmtFrom + delta : start(moved.stmtFrom)
      moved.stmtTo = i > edited ? moved.stmtTo + delta : end(moved.stmtTo)
    }
    return moved
  })
}
