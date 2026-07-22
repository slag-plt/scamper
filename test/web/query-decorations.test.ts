import { EditorState } from "@codemirror/state"
import { EditorView } from "@codemirror/view"
import { afterEach, describe, expect, test } from "vitest"
import { Range } from "../../src/lpm"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import Scamper, { initialize } from "../../src/scamper"
import {
  QueryExtension,
  syncQueryDecorations,
} from "../../src/web/codemirror/extensions/query"

await initialize()

describe("query decorations", () => {
  afterEach(() => {
    Scamper.getInstance().invalidateAllQueries()
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

    const scamper = Scamper.getInstance()
    scamper.registerQueryEntry({
      id: "test-query",
      queriedRange: Range.of(1, 8, 8, 1, 8, 8),
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
