import type { EditorView } from "@codemirror/view"
import { Loc } from "../../lpm"
import { QUERIES_CHANGED, ScamperInstance } from "../../scamper"
import {
  mkFreshEditorState,
  mkNoFileEditorState,
} from "../codemirror/codemirror"
import { syncQueryDecorations } from "../codemirror/extensions/query"

export function createCodeMirrorEditorAdapter(
  view: EditorView,
  dirtyAction: () => void,
) {
  let loaded = false
  const scamper = ScamperInstance.getInstance()
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
          isReadOnly: false,
        }),
      )
    },

    initializeDummyDoc() {
      loaded = false
      view.setState(mkNoFileEditorState())
    },

    getCursorLoc() {
      const idx = view.state.selection.main.head
      const line = view.state.doc.lineAt(idx)
      return new Loc(line.number, idx - line.from, idx)
    },

    destroy() {
      scamper.queryEvents.removeEventListener(QUERIES_CHANGED, onQueriesChanged)
    },
  }
}

export type CodeMirrorEditorAdapter = ReturnType<
  typeof createCodeMirrorEditorAdapter
>
