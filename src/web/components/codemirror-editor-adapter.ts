import type { EditorView } from "@codemirror/view"
import type { Extension } from "@codemirror/state"
import { Loc } from "../../lpm"
import { mkFreshEditorState, mkNoFileEditorState } from "../codemirror"
import { type PopupCoords, toPopupCoords } from "./query/query-modal-extension"

export function createCodeMirrorEditorAdapter(
  view: EditorView,
  dirtyAction: () => void,
  query: {
    extraExtensions: Extension[]
    subscribe: (listener: () => void) => () => void
  },
) {
  return {
    getDoc() {
      return view.state.doc.toString()
    },

    initializeDoc(src: string) {
      view.setState(
        mkFreshEditorState(
          src,
          {
            dirtyAction,
            isReadOnly: false,
          },
          query.extraExtensions,
        ),
      )
    },

    initializeDummyDoc() {
      view.setState(mkNoFileEditorState(query.extraExtensions))
    },

    getCursorLoc() {
      const idx = view.state.selection.main.head
      const line = view.state.doc.lineAt(idx)
      return new Loc(line.number, idx - line.from, idx)
    },

    coordsAtPos(pos: number): PopupCoords | null {
      return toPopupCoords(view.coordsAtPos(pos))
    },

    onViewChange(listener: () => void): () => void {
      return query.subscribe(listener)
    },
  }
}

export type CodeMirrorEditorAdapter = ReturnType<
  typeof createCodeMirrorEditorAdapter
>
