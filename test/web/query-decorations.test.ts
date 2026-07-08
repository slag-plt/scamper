import { EditorState } from "@codemirror/state"
import { EditorView } from "@codemirror/view"
import { afterEach, describe, expect, test } from "vitest"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import { ScamperInstance } from "../../src/scamper"
import {
  QueryExtension,
  syncQueryDecorations,
} from "../../src/web/codemirror/extensions/query"

describe("query decorations", () => {
  afterEach(() => {
    ScamperInstance.getInstance().invalidateAllQueries()
  })

  test("syncQueryDecorations updates the field from scamper queries", () => {
    const parent = document.createElement("div")
    const view = new EditorView({
      state: EditorState.create({
        doc: "(define x 1)",
        extensions: [QueryExtension],
      }),
      parent,
    })

    const scamper = ScamperInstance.getInstance()
    scamper.registerQueryEntry({
      id: "test-query",
      queryPos: 8,
      err: new SimpleErrorChannel(),
      done: Promise.resolve(),
    })

    syncQueryDecorations(view)

    expect(view.state.field(QueryExtension).size).toBe(1)

    scamper.invalidateAllQueries()
    syncQueryDecorations(view)

    expect(view.state.field(QueryExtension).size).toBe(0)

    view.destroy()
  })
})
