import {
  drawSelection,
  EditorView,
  highlightSpecialChars,
  keymap,
  type KeyBinding,
} from '@codemirror/view'
import { EditorState, type Extension } from '@codemirror/state'
import { bracketMatching, indentOnInput } from '@codemirror/language'
import {
  defaultKeymap,
  history,
  historyKeymap,
  insertNewlineAndIndent,
} from '@codemirror/commands'
import { closeBrackets, closeBracketsKeymap } from '@codemirror/autocomplete'
import { markdown } from '@codemirror/lang-markdown'
import type { Diagnostic } from '@codemirror/lint'
import { currentTheme } from '../../../theme'
import { editorFontSize } from '../editor-prefs'
import {
  editorThemeCompartment,
  editorThemeExtension,
  fontSizeCompartment,
  fontSizeExtension,
} from './codemirror'
import { ScamperLanguage, ScamperSupport } from './extensions/language'
import { scamperLspExtensions } from './lsp'

/**
 * A one-form editor: the box a REPL entry is typed into (#399), and the box a
 * notebook cell will be.
 *
 * Far smaller than {@link mkFreshEditorState}. There are no line numbers, no
 * fold gutter and no lint gutter, because a cell is a few lines rather than a
 * file, and none of the machinery that is about the file as an artifact --
 * inline queries, `@example` checks, the formatter. What it keeps is what makes
 * Scheme bearable to type: highlighting, bracket matching and closing, and
 * DrRacket indentation, all from the same language support the main editor
 * uses, plus -- for a cell given a `lspUri` -- hover docs, completion and
 * signature help from the same language server.
 */

/**
 * Whether `src` closes everything it opens.
 *
 * What decides between Enter running the entry and Enter continuing it onto
 * another line: a half-typed `(define f (lambda (x)` is not something to run.
 * Read off the Lezer tree, so strings, comments and `#\(` are handled by the
 * grammar rather than by a bracket count that would get them wrong.
 *
 * A form that is complete but *wrong* -- a stray `)` -- also parses with an
 * error, and so reads as incomplete here. That would trap the entry with no
 * way to submit it, which is what the blank-line escape in {@link submitKey}
 * is for.
 */
export function isCompleteForm(src: string): boolean {
  let complete = true
  ScamperLanguage.parser.parse(src).iterate({
    enter: (node) => {
      if (node.type.isError) {
        complete = false
        return false
      }
      return true
    },
  })
  return complete
}

/**
 * What a mounted cell offers whatever is holding it: enough to drive the live
 * prompt, and nothing about the editor underneath.
 */
export interface CellEditorHandle {
  /** Replaces the contents, leaving the caret at the end. */
  setText: (text: string) => void
  clear: () => void
  /** @param at which end the caret goes to; the end of the cell by default. */
  focus: (at?: 'start' | 'end') => void
  text: () => string
  /** Underlines what is wrong in this cell, in the cell's own coordinates. */
  setDiagnostics: (diagnostics: Diagnostic[]) => void
}

/** One edit made in a cell, in the cell's own coordinates. */
export interface CellChange {
  from: number
  to: number
  insert: string
}

export interface CellEditorConfig {
  /** Read the entries already run; a live prompt leaves this false. */
  isReadOnly?: boolean
  /** Runs the cell. Called with its text, which the caller usually clears. */
  onSubmit?: (text: string) => void
  /**
   * Told what changed, so a notebook can write the same edit through to the
   * file the cell is a view of (#410).
   *
   * The changes themselves rather than the new text: a cell is a stretch of a
   * document, and replacing the stretch on every keystroke would throw away
   * what the rest of the editor knows about it.
   */
  onChange?: (changes: CellChange[]) => void
  /** Told when the caret enters or leaves, so a notebook can follow it. */
  onFocusChange?: (focused: boolean) => void
  /**
   * What the cell is written in. Scamper unless it is a prose cell, which is
   * Markdown and gets none of the Scheme editing behaviour.
   */
  language?: 'scamper' | 'markdown'
  /**
   * Asked for the previous (-1) or next (1) entry when the caret is on the
   * first or last line and would otherwise leave the cell.
   * @returns whether it moved, so an unhandled edge falls through to
   *          CodeMirror's own cursor motion.
   */
  onHistory?: (direction: -1 | 1) => boolean
  /**
   * The document URI this cell holds, which turns the language server on for
   * it: hover docs, completion and signature help, against the context the
   * cell is set in (see `setLspContext`).
   *
   * One per live cell -- two editors on one URI are two editors overwriting one
   * document. Omitted for a cell nobody is typing in, which is every entry
   * already run: a record does not need completion, and each one would be
   * another document for the server to hold.
   */
  lspUri?: string
}

/**
 * Enter: run the cell, or continue it onto another line.
 *
 * Continuing is the default for anything unclosed, which is what makes a
 * multi-line lambda typeable. Pressing Enter on a line that is already blank
 * runs it regardless -- the escape hatch for source the grammar cannot make
 * sense of, which would otherwise be impossible to submit and impossible to
 * get an error message about.
 */
function submitKey(onSubmit: (text: string) => void): KeyBinding {
  return {
    key: 'Enter',
    run: (view) => {
      const text = view.state.doc.toString()
      // Nothing to run. Swallowed rather than passed on, so a stray Enter does
      // not open a line in an empty prompt.
      if (text.trim().length === 0) return true
      if (isCompleteForm(text) || /\n[ \t]*$/.test(text)) {
        onSubmit(text)
        return true
      }
      return insertNewlineAndIndent(view)
    },
    // Always a newline, for the entry that is complete but not finished being
    // written -- adding an argument to a call that already parses.
    shift: insertNewlineAndIndent,
  }
}

/** Up/Down walk the entry history, but only from the edges of the cell. */
function historyKeys(onHistory: (direction: -1 | 1) => boolean): KeyBinding[] {
  const atEdge = (view: EditorView, wanted: 'first' | 'last') => {
    const { head } = view.state.selection.main
    const line = view.state.doc.lineAt(head)
    return wanted === 'first' ? line.number === 1 : line.number === view.state.doc.lines
  }
  return [
    {
      key: 'ArrowUp',
      run: (view) => (atEdge(view, 'first') ? onHistory(-1) : false),
    },
    {
      key: 'ArrowDown',
      run: (view) => (atEdge(view, 'last') ? onHistory(1) : false),
    },
  ]
}

/** Reports edits and focus, for a cell that is a view of a document. */
function reporters(config: CellEditorConfig): Extension {
  const { onChange, onFocusChange } = config
  if (onChange === undefined && onFocusChange === undefined) return []
  return EditorView.updateListener.of((update) => {
    if (onChange !== undefined && update.docChanged) {
      const changes: CellChange[] = []
      update.changes.iterChanges((from, to, _fromB, _toB, inserted) => {
        changes.push({ from, to, insert: inserted.toString() })
      })
      onChange(changes)
    }
    if (onFocusChange !== undefined && update.focusChanged) {
      onFocusChange(update.view.hasFocus)
    }
  })
}

function cellExtensions(config: CellEditorConfig): Extension {
  const {
    isReadOnly = false,
    onSubmit,
    onHistory,
    lspUri,
    language = 'scamper',
  } = config
  return [
    highlightSpecialChars(),
    history(),
    drawSelection(),
    indentOnInput(),
    bracketMatching(),
    closeBrackets(),
    // A cell is as wide as the window it is in and as long as it needs to be,
    // so a long entry wraps rather than scrolling sideways under the output.
    EditorView.lineWrapping,
    editorThemeCompartment.of(editorThemeExtension(currentTheme.value)),
    fontSizeCompartment.of(fontSizeExtension(editorFontSize.value)),
    // Ahead of the defaults, which bind Enter and the arrows to their usual
    // jobs; these fall through by returning false where they do not apply.
    keymap.of([
      ...(onSubmit ? [submitKey(onSubmit)] : []),
      ...(onHistory ? historyKeys(onHistory) : []),
    ]),
    keymap.of([...closeBracketsKeymap, ...defaultKeymap, ...historyKeymap]),
    EditorState.readOnly.of(isReadOnly),
    // A cell that has been run is a record of what was typed, not a box: it
    // can be selected and copied but not walked through with a caret.
    EditorView.editable.of(!isReadOnly),
    reporters(config),
    language === 'markdown' ? markdown() : ScamperSupport(),
    // The same language services the file editor gets, minus the diagnostics:
    // the server does not lint a document that has a context, since a cell is
    // unclosed for most of the time it is being typed.
    lspUri === undefined || language === 'markdown'
      ? []
      : scamperLspExtensions(lspUri),
  ]
}

/** The state for one cell. */
export function mkCellEditorState(
  doc: string,
  config: CellEditorConfig = {},
): EditorState {
  return EditorState.create({ doc, extensions: cellExtensions(config) })
}
