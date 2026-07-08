import type { Text } from "@codemirror/state"
import { StateEffect, StateField, Transaction } from "@codemirror/state"
import { Decoration, DecorationSet, EditorView, WidgetType } from "@codemirror/view"
import { type QueryEntry, ScamperInstance } from "../../../scamper"

export function getIdForGhostLine(line: number) {
  return `query-ghost-line-${line.toString()}`
}

class GhostLineWidget extends WidgetType {
  constructor(readonly line: number) {
    super()
  }

  toDOM(): HTMLElement {
    const containerEl = document.createElement("div")
    containerEl.id = getIdForGhostLine(this.line)
    containerEl.style.height = "1lh"
    return containerEl
  }

  eq(other: WidgetType): boolean {
    return other instanceof GhostLineWidget && other.line === this.line
  }
}

function buildGhostLines(
  queries: readonly QueryEntry[],
  doc: Text,
): DecorationSet {
  if (doc.length === 0) {
    return Decoration.none
  }
  const lines = queries.reduce((lines, q) => {
    lines.add(q.queryLoc.line)
    return lines
  }, new Set<number>())
  const widgets = [...lines].map((line) =>
    Decoration.widget({
      widget: new GhostLineWidget(line),
      block: true,
    }).range(doc.line(line).from),
  )
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
