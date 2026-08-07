import type { EditorView } from '@codemirror/view'
import { diff } from '@codemirror/merge'
import { Loc } from '../../../lpm'
import Scamper, { QUERIES_CHANGED } from '../../../scamper'
import {
  mkFreshEditorState,
  mkNoFileEditorState,
} from '../codemirror/codemirror'
import { lineColumnAt, type CursorStatus } from '../codemirror/enclosing-form'
import { syncQueryDecorations } from '../codemirror/extensions/query'

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

    destroy() {
      scamper.queryEvents.removeEventListener(QUERIES_CHANGED, onQueriesChanged)
    },
  }
}

export type CodeMirrorEditorAdapter = ReturnType<
  typeof createCodeMirrorEditorAdapter
>
