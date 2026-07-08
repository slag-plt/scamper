import type { Text } from "@codemirror/state"
import { StateEffect, StateField, Transaction } from "@codemirror/state"
import {
  Decoration,
  DecorationSet,
  EditorView,
  WidgetType,
} from "@codemirror/view"
import { type QueryEntry, ScamperInstance } from "../../../scamper"

class GhostLineWidget extends WidgetType {
  toDOM(): HTMLElement {
    const containerEl = document.createElement("div")
    containerEl.style.height = "1lh"
    return containerEl
  }
}

function buildGhostLines(
  queries: readonly QueryEntry[],
  doc: Text,
): DecorationSet {
  const widgets = queries.flatMap((q) => {
    if (doc.length === 0) return []
    const pos = Math.min(q.queryPos, doc.length)
    return [
      Decoration.widget({
        widget: new GhostLineWidget(),
        block: true,
      }).range(doc.lineAt(pos).from),
    ]
  })
  return Decoration.set(widgets, true)
}

export const queryDecorationsSet = StateEffect.define<DecorationSet>()

export function syncQueryDecorations(view: EditorView) {
  view.dispatch({
    effects: queryDecorationsSet.of(
      buildGhostLines(ScamperInstance.getInstance().queries, view.state.doc),
    ),
    annotations: Transaction.addToHistory.of(false),
  })
}

export const QueryExtension = StateField.define<DecorationSet>({
  create(state) {
    return buildGhostLines(ScamperInstance.getInstance().queries, state.doc)
  },
  update(deco, tr) {
    for (const e of tr.effects) {
      if (e.is(queryDecorationsSet)) return e.value
    }
    if (tr.docChanged) {
      return buildGhostLines(
        ScamperInstance.getInstance().queries,
        tr.state.doc,
      )
    }
    return deco.map(tr.changes)
  },
  provide: (f) => EditorView.decorations.from(f),
})
