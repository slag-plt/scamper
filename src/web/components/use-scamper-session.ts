import {
  computed,
  inject,
  provide,
  reactive,
  ref,
  type ComputedRef,
  type InjectionKey,
  type Ref,
  type ShallowRef,
} from "vue"
import { ScamperInstance, type DisplayRun } from "../../scamper-instance"
import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import type { SchedulerId } from "../../scheduler"
import type { ResultsPaneType } from "./use-results-pane"
import type { EditorAccessor } from "./editor-context"

interface QueryEntry {
  id: SchedulerId
  targetPos: number
  err: SimpleErrorChannel
}

export interface ScamperSessionOptions {
  editor: EditorAccessor
  onRunScheduled?: () => void
}

export interface ScamperSession {
  queries: Ref<QueryEntry[]>
  currentRun: ComputedRef<SchedulerId | null>
  isTracing: ComputedRef<boolean>
  resetOutput(): void
  stopRun(): void
  closeAllQueries(): void
  closeQuery(id: SchedulerId): void
  stopAll(): void
  execute(options?: { tracing?: boolean }): void
  query(): void
}

const ScamperSessionKey: InjectionKey<ScamperSession> = Symbol("ScamperSession")

function createScamperSession(
  pane: ShallowRef<ResultsPaneType | null>,
  editor: EditorAccessor,
  onRunScheduled?: () => void,
): ScamperSession {
  const queries = ref<QueryEntry[]>([])
  const activeRun = ref<DisplayRun | null>(null)
  const scamper = ScamperInstance.getInstance()

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

  function closeAllQueries() {
    for (const q of queries.value) {
      scamper.cancel(q.id)
    }
    queries.value = []
  }

  function closeQuery(id: SchedulerId) {
    scamper.cancel(id)
    queries.value = queries.value.filter((q) => q.id !== id)
  }

  function stopAll() {
    closeAllQueries()
    stopRun()
  }

  const execute = ({ tracing = false }: { tracing?: boolean } = {}) => {
    const ch = display()
    if (!ch) return

    stopRun()
    resetOutput()

    const src = editor().getDoc()
    const run = scamper.execute({
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

  const query = () => {
    const err = reactive(new SimpleErrorChannel())
    const queryLoc = editor().getCursorLoc()
    const src = editor().getDoc()
    const id = scamper.query({ src, err, queryLoc })
    if (id) {
      queries.value.push({ id, targetPos: queryLoc.idx, err })
    }
    onRunScheduled?.()
  }

  return {
    queries,
    currentRun,
    isTracing,
    resetOutput,
    stopRun,
    closeAllQueries,
    closeQuery,
    stopAll,
    execute,
    query,
  }
}

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

export type { QueryEntry }
