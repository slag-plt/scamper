import {
  computed,
  inject,
  onScopeDispose,
  type InjectionKey,
  provide,
  reactive,
  ref,
  shallowRef,
  type ShallowRef,
} from "vue"
import {
  type DisplayRequest,
  type QueryMap,
  QUERIES_CHANGED,
  ScamperInstance,
} from "../../scamper"
import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import type { SchedulerId } from "../../scheduler"
import type { ResultsPaneType } from "./use-results-pane"
import type { EditorAccessor } from "./editor-context"

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
  const scamper = ScamperInstance.getInstance()

  const queries = shallowRef<QueryMap>(new Map(scamper.queries))

  const syncQueries = () => {
    queries.value = new Map(scamper.queries)
  }

  scamper.queryEvents.addEventListener(QUERIES_CHANGED, syncQueries)
  onScopeDispose(() => {
    scamper.queryEvents.removeEventListener(QUERIES_CHANGED, syncQueries)
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
    currentRun,
    isTracing,
    resetOutput,
    stopRun,
    invalidateAllQueries,
    invalidateQuery,
    stopAll,
    execute,
    query,
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
