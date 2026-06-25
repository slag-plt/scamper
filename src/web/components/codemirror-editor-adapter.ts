import type { EditorView } from "@codemirror/view"
import { Loc } from "../../lpm"
import { mkFreshEditorState, mkNoFileEditorState } from "../codemirror"

export function createCodeMirrorEditorAdapter(
  view: EditorView,
  dirtyAction: () => void,
) {
  return {
    getDoc() {
      return view.state.doc.toString()
    },

    initializeDoc(src: string) {
      view.setState(
        mkFreshEditorState(src, {
          dirtyAction,
          isReadOnly: false,
        }),
      )
    },

    initializeDummyDoc() {
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
