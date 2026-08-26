import {
  computed,
  inject,
  type InjectionKey,
  onScopeDispose,
  provide,
  reactive,
  ref,
  type Ref,
  shallowRef,
} from 'vue'
import Scamper, {
  type DisplayRequest,
  QUERIES_CHANGED,
  QUERY_EXPANDED_CHANGED,
  type QueryMap,
} from '../../../scamper'
import { SimpleErrorChannel } from '../../../lpm/output/simple-error'
import type { SchedulerId } from '../../../lpm/scheduler'
import type { ResultsPaneType } from './use-results-pane'
import type { EditorAccessor } from './editor-context'
import { throwNull } from '../../../utils'

export interface ScamperSessionOptions {
  editor: EditorAccessor
  onRunScheduled?: () => void
  /**
   * Called with the source that was run, once a run has finished on its own.
   * A run that was superseded or stopped never reaches this. What checks the
   * file's `@example` lines hangs off it (issue #374).
   */
  onRunSettled?: (src: string) => void
}

/**
 * @param pane where a run's output goes. Not always the output pane: in the
 *        notebook view it is the notebook, which takes the same three
 *        operations (#410). A ref rather than a value, since the person can
 *        change their mind between one run and the next.
 */
function createScamperSession(
  pane: Readonly<Ref<ResultsPaneType | null>>,
  editor: EditorAccessor,
  onRunScheduled?: () => void,
  onRunSettled?: (src: string) => void,
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

  const execute = async ({
    tracing = false,
    stepping = false,
  }: { tracing?: boolean; stepping?: boolean } = {}) => {
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
      stepping,
    })
    if (!run) {
      activeRun.value = null
      onRunScheduled?.()
      // The program did not compile, so there is nothing left of the last run
      // to keep on screen.
      onRunSettled?.(src)
      return
    }

    activeRun.value = run
    const runId = run.id
    void run.done.finally(() => {
      // Guarded so a run that a newer one superseded says nothing: what
      // listens is about the file as it is now.
      if (activeRun.value?.id !== runId) return
      activeRun.value = null
      onRunSettled?.(src)
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

  // Step controls for a stepping run (see execute({ stepping: true })). They
  // target the active run's id; no-op when nothing is running.
  const step = () => {
    const id = currentRun.value
    if (id) scamper.step(id)
  }
  const stepStmt = async () => {
    const id = currentRun.value
    if (id) await scamper.resume(id, 'statement')
  }
  const stepAll = async () => {
    const id = currentRun.value
    if (id) await scamper.resume(id, 'all')
  }
  // Stop an in-flight statement/all burst, re-pausing at the next reduction
  // (keeps the stepping session alive, unlike stopRun which cancels the run).
  const abortStep = () => {
    const id = currentRun.value
    if (id) scamper.pauseStepping(id)
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
    step,
    stepStmt,
    stepAll,
    abortStep,
    getQueryOrThrow,
  }
}

export type ScamperSession = ReturnType<typeof createScamperSession>

const ScamperSessionKey: InjectionKey<ScamperSession> = Symbol('ScamperSession')

export function provideScamperSession(
  pane: Readonly<Ref<ResultsPaneType | null>>,
  options: ScamperSessionOptions,
): ScamperSession {
  const session = createScamperSession(
    pane,
    options.editor,
    options.onRunScheduled,
    options.onRunSettled,
  )
  provide(ScamperSessionKey, session)
  return session
}

export function useScamperSession(): ScamperSession {
  const session = inject(ScamperSessionKey)
  if (!session) {
    throw new Error(
      'Scamper session missing: call provideScamperSession() in an ancestor',
    )
  }
  return session
}
