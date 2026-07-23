import { computed, defineComponent, shallowRef } from 'vue'
import { flushPromises, mount } from '@vue/test-utils'
import { afterEach, describe, expect, test, vi } from 'vitest'
import { LoggingChannel, Range, ReportError } from '../../src/lpm'
import Scamper, { initialize } from '../../src/scamper'
import type { EditorAccessor } from '../../src/app/web/composables/editor-context'
import {
  provideScamperSession,
  type ScamperSession,
} from '../../src/app/web/composables/use-scamper-session'
import type { ResultsPaneType } from '../../src/app/web/composables/use-results-pane'
import { makeMockCodeMirrorEditorAdapter } from '../stubs/mock-code-mirror-editor-adapter'

await initialize()

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
describe('query modal reactivity regression', () => {
  let session!: ScamperSession
  let reportQueryResult: ((value: number) => void) | null = null

  afterEach(() => {
    Scamper.getInstance().invalidateAllQueries()
    vi.restoreAllMocks()
    reportQueryResult = null
  })

  function mountQueryStatusHost() {
    const editor: EditorAccessor = () => makeMockCodeMirrorEditorAdapter()

    const Host = defineComponent({
      setup() {
        const paneRef = shallowRef<ResultsPaneType | null>(makePane())
        session = provideScamperSession(paneRef, { editor })
        const scamper = Scamper.getInstance()
        vi.spyOn(scamper, 'query').mockImplementation(({ err }) => {
          reportQueryResult = (value: number) => {
            err.report(new ReportError(value, Range.none))
          }
          scamper.registerQueryEntry({
            id: 'query-test',
            err,
            queriedRange: Range.of(0, 0, 0, 0, 0, 0),
            done: Promise.resolve(),
          })
          return Promise.resolve()
        })
        return { queries: computed(() => session.queries.value) }
      },
      template: `
        <div>
          <template v-for="[line, bucket] in queries" :key="line">
            <span
              v-for="q in bucket"
              :key="q.id"
              data-testid="query-status"
            >
              {{ q.err.errors.length === 0 ? "pending" : "done:" + q.err.errors.length }}
            </span>
          </template>
        </div>
      `,
    })

    return mount(Host)
  }

  test('async query report updates the modal without a scroll/repaint', async () => {
    const wrapper = mountQueryStatusHost()

    await session.query()
    await flushPromises()
    expect(wrapper.get('[data-testid="query-status"]').text()).toBe('pending')

    expect(reportQueryResult).not.toBeNull()
    reportQueryResult?.(42)
    await flushPromises()

    expect(wrapper.get('[data-testid="query-status"]').text()).toBe('done:1')
  })
})
