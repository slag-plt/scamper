import type { EditorView } from "@codemirror/view"
import { Loc } from "../../lpm"
import {
  mkFreshEditorState,
  mkNoFileEditorState,
} from "../codemirror/codemirror"

export function createCodeMirrorEditorAdapter(
  view: EditorView,
  dirtyAction: () => void,
) {
  let loaded = false

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
  }
}

export type CodeMirrorEditorAdapter = ReturnType<
  typeof createCodeMirrorEditorAdapter
>
