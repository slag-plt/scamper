import type { EditorView } from '@codemirror/view'
import { diff } from '@codemirror/merge'
import {
  redo,
  redoDepth,
  toggleComment,
  undo,
  undoDepth,
} from '@codemirror/commands'
import { foldAll, unfoldAll } from '@codemirror/language'
import { gotoLine, openSearchPanel } from '@codemirror/search'
import { findReferences, jumpToDefinition } from '@codemirror/lsp-client'
import { Loc } from '../../../lpm'
import Scamper, { QUERIES_CHANGED } from '../../../scamper'
import {
  mkFreshEditorState,
  mkNoFileEditorState,
} from '../codemirror/codemirror'
import { formatScamperDocument } from '../codemirror/extensions/prettier'
import { lineColumnAt, type CursorStatus } from '../codemirror/enclosing-form'
import { syncQueryDecorations } from '../codemirror/extensions/query'
import { identifierAt } from '../../../scheme/token'

/** Cursor status reported when no code is under the cursor (top of document). */
const TOP_LEVEL: CursorStatus = { line: 1, column: 1, path: [] }

export function createCodeMirrorEditorAdapter(
  view: EditorView,
  dirtyAction: () => void,
  onCursorChange?: (status: CursorStatus) => void,
) {
  let loaded = false
  const scamper = Scamper.getInstance()
  const onQueriesChanged = () => {
    syncQueryDecorations(view)
  }

  scamper.queryEvents.addEventListener(QUERIES_CHANGED, onQueriesChanged)
  syncQueryDecorations(view)

  return {
    getDoc() {
      return view.state.doc.toString()
    },

    isLoaded() {
      return loaded
    },

    initializeDoc(src: string) {
      loaded = true
      view.setState(
        mkFreshEditorState(src, {
          dirtyAction,
          onCursorChange,
          isReadOnly: false,
        }),
      )
      // setState doesn't fire update listeners; the cursor resets to the top of
      // the document, so report the top-level status.
      onCursorChange?.(TOP_LEVEL)
    },

    initializeDummyDoc() {
      loaded = false
      view.setState(mkNoFileEditorState(onCursorChange))
      onCursorChange?.(TOP_LEVEL)
    },

    /**
     * Replaces the document with `src` as an ordinary edit rather than a fresh
     * state: a minimal change set derived from a diff, so the cursor keeps its
     * place, the change is undoable, and it marks the file dirty like any
     * other. Used to restore a snapshot from the file's history.
     */
    replaceDoc(src: string) {
      const current = view.state.doc.toString()
      if (current === src) return
      const changes = diff(current, src).map((c) => ({
        from: c.fromA,
        to: c.toA,
        insert: src.slice(c.fromB, c.toB),
      }))
      const changeSet = view.state.changes(changes)
      const sel = view.state.selection.main
      view.dispatch({
        changes,
        selection: {
          anchor: changeSet.mapPos(sel.anchor, 1),
          head: changeSet.mapPos(sel.head, 1),
        },
        scrollIntoView: true,
      })
    },

    getCursorLoc() {
      const idx = view.state.selection.main.from
      const { line, columnOffset } = lineColumnAt(view.state.doc, idx)
      return new Loc(line, columnOffset, idx)
    },

    coordsAtIdx(idx: number) {
      return view.coordsAtPos(idx)
    },

    // ---------- editing commands ----------
    //
    // The editor's own keymap already binds all of these; they are exposed here
    // so the menu bar and the right-click menu can invoke them by name instead
    // of each reaching into the EditorView and reimplementing them.

    focus() {
      view.focus()
    },

    /** What the menus need to grey the right items out. */
    status() {
      const sel = view.state.selection.main
      return {
        readOnly: view.state.readOnly,
        hasSelection: !sel.empty,
        canUndo: undoDepth(view.state) > 0,
        canRedo: redoDepth(view.state) > 0,
        // Go-to-definition and find-references only mean something on a name.
        onIdentifier:
          identifierAt(view.state.doc.toString(), sel.head) !== undefined,
      }
    },

    undo() {
      undo(view)
    },
    redo() {
      redo(view)
    },

    copy() {
      const { from, to } = view.state.selection.main
      void navigator.clipboard
        .writeText(view.state.sliceDoc(from, to))
        .catch(() => {
          /* clipboard write unavailable or denied */
        })
    },

    async cut() {
      const { from, to } = view.state.selection.main
      try {
        await navigator.clipboard.writeText(view.state.sliceDoc(from, to))
      } catch {
        return // don't delete the text if it couldn't be copied to the clipboard
      }
      view.dispatch({ changes: { from, to } })
    },

    paste() {
      void navigator.clipboard
        .readText()
        .then((text) => {
          const { from, to } = view.state.selection.main
          view.dispatch({
            changes: { from, to, insert: text },
            selection: { anchor: from + text.length },
          })
        })
        .catch(() => {
          /* clipboard read unavailable or denied */
        })
    },

    selectAll() {
      view.dispatch({ selection: { anchor: 0, head: view.state.doc.length } })
    },

    find() {
      openSearchPanel(view)
    },

    /**
     * Opens the same panel `find` does -- CodeMirror has one search panel and
     * it carries the replace fields -- but puts the cursor in the replace box,
     * which is the whole difference between the two menu items.
     */
    replace() {
      openSearchPanel(view)
      // The panel is added to the DOM by the dispatch above, so the query has
      // to wait for it; requestAnimationFrame is enough and needs no timer.
      requestAnimationFrame(() => {
        view.dom
          .querySelector<HTMLInputElement>('.cm-search input[name="replace"]')
          ?.select()
      })
    },

    goToLine() {
      gotoLine(view)
    },

    toggleComment() {
      toggleComment(view)
    },

    format() {
      formatScamperDocument(view)
    },

    foldAll() {
      foldAll(view)
    },
    unfoldAll() {
      unfoldAll(view)
    },

    goToDefinition() {
      jumpToDefinition(view)
    },
    findReferences() {
      findReferences(view)
    },

    destroy() {
      scamper.queryEvents.removeEventListener(QUERIES_CHANGED, onQueriesChanged)
    },
  }
}

export type CodeMirrorEditorAdapter = ReturnType<
  typeof createCodeMirrorEditorAdapter
>
