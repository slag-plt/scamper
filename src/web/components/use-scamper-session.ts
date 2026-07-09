import { computed, inject, type InjectionKey, provide, reactive, ref, type ShallowRef } from "vue"
import { type DisplayRequest, Scamper } from "../../scamper"
import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import type { SchedulerId } from "../../lpm/scheduler"
import type { ResultsPaneType } from "./use-results-pane"
import type { EditorAccessor } from "./editor-context"

interface QueryEntry {
  id: SchedulerId
  targetPos: number
  err: SimpleErrorChannel
  done: Promise<void>
}

export interface ScamperSessionOptions {
  editor: EditorAccessor
  onRunScheduled?: () => void
}

function createScamperSession(
  pane: ShallowRef<ResultsPaneType | null>,
  editor: EditorAccessor,
  onRunScheduled?: () => void,
) {
  const queries = ref<QueryEntry[]>([])
  const activeRun = ref<DisplayRequest | null>(null)
  const scamper = Scamper.getInstance()

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
    const run = await scamper.query({ src, err, queryLoc })
    if (run) {
      queries.value.push({
        id: run.id,
        targetPos: queryLoc.idx,
        err,
        done: run.done,
      })
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

export type { QueryEntry }
