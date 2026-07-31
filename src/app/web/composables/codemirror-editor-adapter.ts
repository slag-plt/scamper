import type { EditorView } from '@codemirror/view'
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
