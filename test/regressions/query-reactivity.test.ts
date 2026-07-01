/* eslint-disable vue/one-component-per-file */
import { defineComponent, shallowRef } from "vue"
import { flushPromises, mount } from "@vue/test-utils"
import { afterEach, describe, expect, test, vi } from "vitest"
import { Loc, Range, ReportError } from "../../src/lpm"
import { ScamperInstance } from "../../src/scamper"
import type { CodeMirrorEditorAdapter } from "../../src/web/components/codemirror-editor-adapter"
import type { EditorAccessor } from "../../src/web/components/editor-context"
import {
  provideScamperSession,
  type ScamperSession,
} from "../../src/web/components/use-scamper-session"
import type { ResultsPaneType } from "../../src/web/components/use-results-pane"
import { LoggingChannel } from "../../src/lpm"

function makeAdapter(): CodeMirrorEditorAdapter {
  return {
    getDoc: () => "1",
    isLoaded: () => true,
    initializeDoc: () => {
      /* noop */
    },
    initializeDummyDoc: () => {
      /* noop */
    },
    getCursorLoc: () => new Loc(0, 0, 0),
    coordsAtPos: () => null,
    onViewChange: () => () => {
      /* noop */
    },
  }
}

function makePane(): ResultsPaneType {
  const ch = new LoggingChannel(false, false)
  return { display: ch, reset: vi.fn(), scrollToBottom: vi.fn() }
}

/**
 * Regression: after querying an @example-tagged define, the modal stays on
 * "Queried code could not be reached!" until scroll forces a repaint.
 * Root cause: async SimpleErrorChannel.report() did not trigger Vue updates.
 * Fix: wrap the channel in reactive() so err.errors is tracked.
 */
describe("query modal reactivity regression", () => {
  let session!: ScamperSession
  let reportQueryResult: ((value: number) => void) | null = null

  afterEach(() => {
    vi.restoreAllMocks()
    reportQueryResult = null
  })

  function mountQueryStatusHost() {
    const editor: EditorAccessor = () => makeAdapter()

    const Host = defineComponent({
      setup() {
        const paneRef = shallowRef<ResultsPaneType | null>(makePane())
        session = provideScamperSession(paneRef, { editor })
        vi.spyOn(ScamperInstance.getInstance(), "query").mockImplementation(
          ({ err }) => {
            reportQueryResult = (value: number) => {
              err.report(new ReportError(value, Range.none))
            }
            return "query-1"
          },
        )
        return { queries: session.queries }
      },
      template: `
        <div>
          <template v-for="q in queries" :key="q.id">
            <span data-testid="query-status">
              {{ q.err.errors.length === 0 ? "pending" : "done:" + q.err.errors.length }}
            </span>
          </template>
        </div>
      `,
    })

    return mount(Host)
  }

  test("async query report updates the modal without a scroll/repaint", async () => {
    const wrapper = mountQueryStatusHost()

    session.query()
    await flushPromises()
    expect(wrapper.get('[data-testid="query-status"]').text()).toBe("pending")

    expect(reportQueryResult).not.toBeNull()
    reportQueryResult!(42)
    await flushPromises()

    expect(wrapper.get('[data-testid="query-status"]').text()).toBe("done:1")
  })
})
