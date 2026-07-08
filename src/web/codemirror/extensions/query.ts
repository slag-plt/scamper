import type { Text } from "@codemirror/state"
import { Transaction } from "@codemirror/state"
import { Decoration, DecorationSet, EditorView, ViewPlugin } from "@codemirror/view"
import { QUERIES_CHANGED, type QueryEntry, ScamperInstance } from "../../../scamper"

function buildQueryDecorations(
  queries: readonly QueryEntry[],
  doc: Text,
): DecorationSet {
  const marks = queries.flatMap((q) => {
    if (doc.length === 0) return []
    const pos = Math.min(q.queryPos, doc.length)
    const from = pos < doc.length ? pos : pos - 1
    const to = from + 1
    return [Decoration.mark({ class: "cm-query-placeholder" }).range(from, to)]
  })
  return Decoration.set(marks, true)
}

export const QueryExtension = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet
    #off: () => void
    #alive = true

    constructor(private view: EditorView) {
      this.decorations = Decoration.none
      const sync = (scheduleDispatch: boolean) => {
        this.decorations = buildQueryDecorations(
          ScamperInstance.getInstance().queries,
          this.view.state.doc,
        )
        if (!scheduleDispatch) return
        queueMicrotask(() => {
          if (!this.#alive) return
          this.view.dispatch({
            annotations: Transaction.addToHistory.of(false),
          })
        })
      }
      const scamper = ScamperInstance.getInstance()
      const onQueriesChanged = () => {
        sync(true)
      }
      scamper.queryEvents.addEventListener(QUERIES_CHANGED, onQueriesChanged)
      this.#off = () => {
        scamper.queryEvents.removeEventListener(
          QUERIES_CHANGED,
          onQueriesChanged,
        )
      }
      sync(false)
    }

    destroy() {
      this.#alive = false
      this.#off()
    }
  },
  {
    decorations: (v) => v.decorations,
  },
)
