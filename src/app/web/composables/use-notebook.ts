import {
  computed,
  getCurrentScope,
  onScopeDispose,
  shallowRef,
  type Ref,
} from 'vue'
import type { Diagnostic } from '@codemirror/lint'
import type { Value } from '../../../lpm'
import type { EditorAccessor } from './editor-context'
import { NotebookDisplay } from '../notebook-display'
import {
  captionOf,
  shiftCells,
  splitIntoCells,
  type Cell,
} from '../notebook-cells'
import { analyzeSource } from '../codemirror/lsp/diagnostics'
import type { CellChange } from '../codemirror/cell-editor'

/**
 * The notebook view of the open file (#410).
 *
 * The notebook is a *view* of the document the editor holds, not a second copy
 * of it: the cells are stretches of that document, and editing one writes the
 * same edit through to it. So saving, the file's history, live evaluation and
 * the `@example` checks all keep working without knowing this exists, and the
 * file's own spacing survives being looked at as a notebook.
 *
 * The split is re-taken when the document changes from outside and at the
 * start of every run -- `reset` is what the session calls before running, so a
 * run is always against cells that match the file. It is deliberately *not*
 * re-taken on every keystroke: cells appearing and disappearing under the caret
 * is not something anyone wants while they are typing.
 */

/**
 * A cell, and the identity that keeps its editor alive across re-splits.
 *
 * `isDraft` marks a cell someone asked for that the file does not hold yet: it
 * is empty, so there is nothing in the document to find it by. It becomes an
 * ordinary cell as soon as anything is typed into it.
 */
export type NotebookCell = Cell & { id: number; isDraft?: boolean }

export interface Notebook {
  readonly cells: Ref<NotebookCell[]>
  /** Bumped whenever a run adds output, so a view knows to redraw. */
  readonly version: Ref<number>
  /** What the cell at `index` produced. */
  outputOf: (index: number) => Value[]
  /** Output that belongs to no cell: an error with nowhere to point. */
  readonly unplaced: Readonly<Ref<Value[]>>
  /** What is wrong in each cell, in that cell's own coordinates. */
  readonly diagnostics: Ref<Diagnostic[][]>
  /** Where a run's output goes. Handed to the session in place of the pane. */
  readonly display: NotebookDisplay
  /** Re-reads the document and re-takes the split. */
  refresh: () => void
  /** Empties the output and re-takes the split. Called before every run. */
  reset: () => void
  scrollToBottom: () => void
  /** Writes an edit made in a cell through to the document. */
  applyChanges: (index: number, changes: CellChange[]) => void
  /** Replaces a whole cell, for prose, which is edited as Markdown. */
  replaceCell: (index: number, text: string) => void
  /** Adds an empty cell below `index`, or at the top when it is -1. */
  insertCell: (index: number, kind: 'code' | 'prose') => number
  /** Takes a cell out of the file. */
  removeCell: (index: number) => void
  /** Puts the file's caret where the caret in a cell is. */
  noteCursor: (index: number, pos: number) => void
  /** Notes that the document changed, so the squiggles can catch up. */
  noteEdit: () => void
  /** Drops the timers this holds. */
  cancel: () => void
}

/** How long the typing must stop before the cells are marked up again. */
const LINT_IDLE_MS = 300

export function useNotebook(editor: EditorAccessor): Notebook {
  const cells = shallowRef<NotebookCell[]>([])
  const version = shallowRef(0)
  const diagnostics = shallowRef<Diagnostic[][]>([])
  let nextId = 0
  let lintTimer: ReturnType<typeof setTimeout> | null = null
  /** Bumped by each analysis, so a slower one cannot overwrite a newer one. */
  let lintGeneration = 0

  // Batched the way the output pane batches its blocks: a program printing in
  // a loop should cost one redraw per frame, not one per value.
  let redrawQueued = false
  const display = new NotebookDisplay(() => {
    if (redrawQueued) return
    redrawQueued = true
    requestAnimationFrame(() => {
      redrawQueued = false
      version.value++
    })
  })

  // Read through `version`, so a view showing it redraws when something lands
  // in it: the array itself is filled in place as a run proceeds.
  const unplaced = computed(() => {
    void version.value
    return display.unplaced
  })

  /**
   * The cell at `index`, if there is one.
   *
   * Not `cells.value.at`, whose negative indices count from the end: -1 here
   * means "above the first cell", which is a position rather than a cell.
   */
  function cellAt(index: number): NotebookCell | undefined {
    return index < 0 ? undefined : cells.value[index]
  }

  /**
   * The document, or null when there is no editor to ask.
   *
   * There is not always one: before a file is open, and while the IDE is being
   * torn down with a timer still pending. Neither is an error -- there is just
   * nothing to be a view of.
   */
  function doc(): string | null {
    try {
      return editor().getDoc()
    } catch {
      return null
    }
  }

  /** Re-takes the split, keeping the identity of cells that are still there. */
  function refresh(): void {
    const src = doc()
    if (src === null) return
    const fresh = splitIntoCells(src)
    // A file mid-edit usually does not parse. Keeping the split we have is the
    // whole reason the offsets are maintained by hand between splits.
    if (fresh === null) return
    cells.value = reconcile(cells.value, fresh)
    scheduleLint()
  }

  /**
   * Matches the cells we have against the ones we just found, by where they
   * are: a cell that still covers any of the same text is the same cell, and
   * keeps its editor, its caret and its scroll position.
   */
  function reconcile(old: NotebookCell[], fresh: Cell[]): NotebookCell[] {
    const taken = new Set<number>()
    const matched = fresh.map((cell) => {
      const previous = old.find(
        (o) =>
          !taken.has(o.id) &&
          o.kind === cell.kind &&
          o.from < cell.to &&
          cell.from < o.to,
      )
      if (previous === undefined) return { ...cell, id: nextId++ }
      taken.add(previous.id)
      return { ...cell, id: previous.id }
    })
    // A cell someone asked for and has not written anything into yet is in no
    // split, since the file holds nothing to find it by. Put it back where it
    // was asked for.
    const drafts = old.filter((o) => o.isDraft === true && !taken.has(o.id))
    if (drafts.length === 0) return matched
    return [...matched, ...drafts].sort((a, b) => a.from - b.from)
  }

  function slots() {
    const src = doc() ?? ''
    return cells.value.map((cell) => ({
      from: cell.from,
      to: cell.to,
      caption: cell.kind === 'code' ? captionOf(cell, src) : '',
    }))
  }

  function reset(): void {
    refresh()
    display.setSlots(slots())
    version.value++
  }

  /**
   * Applies `changes`, which are in the cell's own coordinates, to the
   * document, and moves the cells below by what they added.
   *
   * One dispatch each: the offsets each change names are into the document as
   * it was before any of them, so each has to be placed after what the ones
   * before it did.
   */
  function applyChanges(index: number, changes: CellChange[]): void {
    const cell = cellAt(index)
    if (cell === undefined) return
    const start = cell.from
    let shifted = 0
    for (const change of changes) {
      const from = start + change.from + shifted
      const to = start + change.to + shifted
      editor().replaceRange(from, to, change.insert)
      const src = doc() ?? ''
      const moved = shiftCells(cells.value, from, to, change.insert.length, src)
      // An edit that no cell contains: the document and the split have parted
      // company, which only a re-split can put right.
      if (moved === null) {
        refresh()
        return
      }
      cells.value = moved
      shifted += change.insert.length - (change.to - change.from)
    }
    // It holds something now, so the file has somewhere to find it.
    const written = cellAt(index)
    if (cell.isDraft === true && written !== undefined && written.to > written.from) {
      cells.value = cells.value.map((c, i) =>
        i === index ? { ...c, isDraft: undefined } : c,
      )
    }
    scheduleLint()
  }

  /** Replaces a cell outright, which is how a prose cell is written back. */
  function replaceCell(index: number, text: string): void {
    const cell = cellAt(index)
    if (cell === undefined || cell.text === text) return
    applyChanges(index, [{ from: 0, to: cell.to - cell.from, insert: text }])
  }

  /**
   * Opens a cell below `index`.
   *
   * A blank line goes in with it, which is the spacing a file of separate
   * top-level forms is written with -- and the only place the notebook decides
   * anything about a file's whitespace, since everywhere else it leaves what
   * is already there alone.
   */
  function insertCell(index: number, kind: 'code' | 'prose'): number {
    const above = cellAt(index)
    const below = cellAt(index + 1)
    const at = above?.to ?? below?.from ?? (doc() ?? '').length
    const separator = cells.value.length === 0 ? '' : '\n\n'
    if (separator.length > 0) editor().replaceRange(at, at, separator)
    // Below a cell the draft goes after the blank line; above the first one it
    // goes before it.
    const from = above === undefined ? at : at + separator.length
    // Re-split rather than shift: nothing but whitespace was added, so the
    // file still parses and the cells come back with their offsets already
    // right.
    refresh()
    const draft: NotebookCell =
      kind === 'code'
        ? {
            kind: 'code',
            from,
            to: from,
            text: '',
            // Nothing will ever be announced for a cell holding no form.
            stmtFrom: from,
            stmtTo: from,
            id: nextId++,
            isDraft: true,
          }
        : { kind: 'prose', from, to: from, text: '', id: nextId++, isDraft: true }
    const position = index + 1
    cells.value = [
      ...cells.value.slice(0, position),
      draft,
      ...cells.value.slice(position),
    ]
    return position
  }

  /** Removes a cell, and the blank line that separated it from the one above. */
  function removeCell(index: number): void {
    const cell = cellAt(index)
    const src = doc()
    if (cell === undefined || src === null) return
    // Back up over the whitespace above it, so removing a cell does not leave
    // a widening gap behind.
    let from = cell.from
    while (from > 0 && /\s/.test(src[from - 1])) from--
    // Unless it is the first cell, where the gap is below it instead.
    let to = cell.to
    if (from === 0) {
      while (to < src.length && /\s/.test(src[to])) to++
    }
    editor().replaceRange(from, to, '')
    refresh()
  }

  /**
   * Follows the caret in a cell with the caret in the file.
   *
   * So the commands that work from where the cursor is -- stepping the
   * statement it is in, querying the value under it, the line and column in
   * the status bar -- mean the same thing in the notebook as in the source
   * view. A prose cell reports its own start: its Markdown and the file's
   * comment lines are not the same text, so a position in one is not a
   * position in the other.
   */
  function noteCursor(index: number, pos: number): void {
    const cell = cellAt(index)
    if (cell === undefined) return
    try {
      editor().setCursor(cell.kind === 'code' ? cell.from + pos : cell.from)
    } catch {
      // No editor to move; there is nothing to keep in step with.
    }
  }

  function noteEdit(): void {
    scheduleLint()
  }

  function scheduleLint(): void {
    if (lintTimer !== null) clearTimeout(lintTimer)
    lintTimer = setTimeout(() => {
      lintTimer = null
      void lint()
    }, LINT_IDLE_MS)
  }

  /**
   * Marks each cell up with what is wrong in it.
   *
   * The whole file is analysed and the results handed out by range, rather than
   * each cell being analysed on its own: a cell half-way through being typed
   * does not parse, and a file's forms lean on each other, so a cell read alone
   * would be marked up with problems it does not have.
   */
  async function lint(): Promise<void> {
    const mine = ++lintGeneration
    const src = doc()
    if (src === null) return
    const found = await analyzeSource(src)
    if (mine !== lintGeneration) return
    const perCell: Diagnostic[][] = cells.value.map(() => [])
    for (const d of found) {
      if (d.range === undefined || d.range.begin.idx < 0) continue
      const at = d.range.begin.idx
      const index = cells.value.findIndex(
        (cell) => at >= cell.from && at < cell.to,
      )
      const cell = cellAt(index)
      if (index === -1 || cell === undefined) continue
      perCell[index].push({
        from: Math.max(0, at - cell.from),
        to: Math.min(cell.to - cell.from, d.range.end.idx + 1 - cell.from),
        severity: d.severity === 'error' ? 'error' : 'warning',
        source: 'scamper',
        message: d.message,
      })
    }
    diagnostics.value = perCell
  }

  function cancel(): void {
    if (lintTimer !== null) clearTimeout(lintTimer)
    lintTimer = null
    // Anything still in flight is about a file that is going away.
    lintGeneration++
  }

  // Guarded so the composable can be exercised without a component around it;
  // in the IDE this is what stops a timer outliving the editor it reads.
  if (getCurrentScope()) {
    onScopeDispose(cancel)
  }

  return {
    cells,
    version,
    unplaced,
    diagnostics,
    display,
    outputOf: (index) => {
      // Read through `version` so a view that calls this re-runs when a run
      // adds something.
      void version.value
      return display.outputOf(index)
    },
    refresh,
    reset,
    scrollToBottom: () => {
      /* a notebook's output is beside the code that made it; there is no
         bottom to go to */
    },
    applyChanges,
    replaceCell,
    insertCell,
    removeCell,
    noteCursor,
    noteEdit,
    cancel,
  }
}
