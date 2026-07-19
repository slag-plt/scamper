import {
  computed,
  inject,
  type InjectionKey,
  onScopeDispose,
  provide,
  reactive,
  ref,
  shallowRef,
  type ShallowRef,
} from "vue"
import Scamper, {
  type DisplayRequest,
  QUERIES_CHANGED,
  QUERY_EXPANDED_CHANGED,
  type QueryMap,
} from "../../scamper"
import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import type { SchedulerId } from "../../lpm/scheduler"
import type { ResultsPaneType } from "./use-results-pane"
import type { EditorAccessor } from "./editor-context"
import { throwNull } from "../../utils"

export interface ScamperSessionOptions {
  editor: EditorAccessor
  onRunScheduled?: () => void
}

function createScamperSession(
  pane: ShallowRef<ResultsPaneType | null>,
  editor: EditorAccessor,
  onRunScheduled?: () => void,
) {
  const activeRun = ref<DisplayRequest | null>(null)
  const scamper = Scamper.getInstance()

  const queries = shallowRef<QueryMap>(scamper.queries)
  const expandedQueryId = shallowRef<SchedulerId | null>(
    scamper.expandedQueryId,
  )

  const syncQueries = () => {
    queries.value = scamper.queries
  }
  const syncExpandedQueryId = () => {
    expandedQueryId.value = scamper.expandedQueryId
  }

  scamper.queryEvents.addEventListener(QUERIES_CHANGED, syncQueries)
  scamper.queryEvents.addEventListener(
    QUERY_EXPANDED_CHANGED,
    syncExpandedQueryId,
  )
  onScopeDispose(() => {
    scamper.queryEvents.removeEventListener(QUERIES_CHANGED, syncQueries)
    scamper.queryEvents.removeEventListener(
      QUERY_EXPANDED_CHANGED,
      syncExpandedQueryId,
    )
  })

  const currentRun = computed(() => activeRun.value?.id ?? null)
  const isTracing = computed(() => activeRun.value?.tracing ?? false)

  function display() {
    return pane.value?.display
  }

  function resetOutput() {
    pane.value?.reset()
  }

  const stopRun = () => {
    const run = activeRun.value
    if (!run) return
    scamper.cancel(run.id)
    activeRun.value = null
  }

  function invalidateAllQueries() {
    scamper.invalidateAllQueries()
  }

  function invalidateQuery(id: SchedulerId) {
    scamper.invalidateQuery(id)
  }

  function expandQuery(id: SchedulerId) {
    console.log("expanding", id)
    scamper.expandQuery(id)
  }

  function collapseQuery() {
    scamper.collapseQuery()
  }

  function toggleQueryExpanded(id: SchedulerId) {
    scamper.toggleQueryExpanded(id)
  }

  function getQueryOrThrow(id: SchedulerId) {
    return scamper.getQuery(id) ?? throwNull(`query ${id} doesn't exist`)
  }

  function stopAll() {
    scamper.invalidateAllQueries()
    stopRun()
  }

  const execute = async ({ tracing = false }: { tracing?: boolean } = {}) => {
    const ch = display()
    if (!ch) return

    stopRun()
    resetOutput()

    const src = editor().getDoc()
    const run = await scamper.execute({
      src,
      out: ch,
      err: ch,
      isTracing: tracing,
    })
    if (!run) {
      activeRun.value = null
      onRunScheduled?.()
      return
    }

    activeRun.value = run
    const runId = run.id
    void run.done.finally(() => {
      if (activeRun.value?.id === runId) {
        activeRun.value = null
      }
    })
    onRunScheduled?.()
  }

  const query = async () => {
    const err = reactive(new SimpleErrorChannel())
    const queryLoc = editor().getCursorLoc()
    const src = editor().getDoc()
    await scamper.query({ src, err, queryLoc })
    onRunScheduled?.()
  }

  return {
    queries,
    expandedQueryId,
    currentRun,
    isTracing,
    resetOutput,
    stopRun,
    invalidateAllQueries,
    invalidateQuery,
    expandQuery,
    collapseQuery,
    toggleQueryExpanded,
    stopAll,
    execute,
    query,
    getQueryOrThrow,
  }
}

export type ScamperSession = ReturnType<typeof createScamperSession>

const ScamperSessionKey: InjectionKey<ScamperSession> = Symbol("ScamperSession")

export function provideScamperSession(
  pane: ShallowRef<ResultsPaneType | null>,
  options: ScamperSessionOptions,
): ScamperSession {
  const session = createScamperSession(
    pane,
    options.editor,
    options.onRunScheduled,
  )
  provide(ScamperSessionKey, session)
  return session
}

export function useScamperSession(): ScamperSession {
  const session = inject(ScamperSessionKey)
  if (!session) {
    throw new Error(
      "Scamper session missing: call provideScamperSession() in an ancestor",
    )
  }
  return session
}
