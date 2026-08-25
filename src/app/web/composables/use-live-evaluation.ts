import { getCurrentScope, onScopeDispose, ref, type Ref } from 'vue'
import { liveEvaluation } from '../run-prefs'

/**
 * Live evaluation (issue #378): the file re-runs by itself shortly after the
 * user stops typing, so the output tracks the code without anyone pressing Run.
 *
 * A file runs when it is opened, so its output is there to read rather than
 * waiting on a keystroke. After that, two timers make up the whole policy:
 *
 * - An *idle* timer, restarted on every edit, so a burst of typing costs one
 *   run rather than one per keystroke.
 * - A *watchdog*, armed on each live run, that stops a program still going
 *   after {@link DEFAULT_RUN_LIMIT_MS}. An infinite loop is an ordinary thing
 *   for a student to write, and without this the first one would leave the IDE
 *   running it forever with no obvious cause.
 *
 * Runs cannot queue up behind each other: at most one idle timer is pending,
 * and starting a run supersedes the one in flight (see `execute` in
 * use-scamper-session.ts), so a program slower than the idle delay is replaced
 * rather than joined.
 *
 * The time limit is deliberately confined to *live* runs. Pressing Run is how a
 * genuinely long program is run in full, and the watchdog's message says so.
 *
 * What it is doing is reported as {@link LiveEvaluation.pending} and
 * {@link LiveEvaluation.liveRunId}, which is what the header's Run control
 * animates from: a run coming, a live run in flight, or neither.
 *
 * Framework-light on purpose -- it takes plain callbacks rather than the
 * session, so its timing can be tested with fake timers and no mounted
 * component.
 */

/** How long the user must stop typing before a live run starts. */
export const DEFAULT_IDLE_MS = 750

/** How long a live run may take before the watchdog stops it. */
export const DEFAULT_RUN_LIMIT_MS = 5000

/**
 * What the header's Run control says live evaluation is doing.
 *
 * - `off` -- turned off; the control is an ordinary Run button.
 * - `idle` -- on, with nothing scheduled and no live run going.
 * - `pending` -- a run is coming, once the typing stops.
 * - `running` -- a live run is in flight.
 */
export type LiveStatus = 'off' | 'idle' | 'pending' | 'running'

export interface LiveEvaluationHooks {
  /** Starts a run of the current file. Resolves once it has been scheduled. */
  run: () => Promise<void> | void
  /** Stops the run in flight, if any. */
  stopRun: () => void
  /** The id of the run in flight, or null if nothing is running. */
  currentRunId: () => string | null
  /**
   * Whether a live run is allowed right now -- as opposed to whether the user
   * wants them at all, which is {@link liveEvaluation} and checked here.
   */
  canRun: () => boolean
  /** Says that a live run was stopped for taking longer than `limitMs`. */
  reportTimeout: (limitMs: number) => void
}

export interface LiveEvaluationOptions {
  idleMs?: number
  runLimitMs?: number
}

export function useLiveEvaluation(
  hooks: LiveEvaluationHooks,
  options: LiveEvaluationOptions = {},
) {
  const idleMs = options.idleMs ?? DEFAULT_IDLE_MS
  const runLimitMs = options.runLimitMs ?? DEFAULT_RUN_LIMIT_MS

  let idleId: ReturnType<typeof setTimeout> | null = null
  let watchdogId: ReturnType<typeof setTimeout> | null = null

  /**
   * Bumped by {@link cancel}, so a run caught in the middle of starting can
   * tell that it is no longer live evaluation's to watch.
   */
  let generation = 0

  /** True while a run is scheduled and has not started yet. */
  const pending: Ref<boolean> = ref(false)

  /**
   * The id of the most recent run started by live evaluation, or null.
   *
   * Left set once the run ends -- nothing tells the composable that it has --
   * so a reader wanting "a live run is in flight" compares this against the
   * run actually in flight, as IdeApp does. That comparison is also what keeps
   * a manual run, which has no time limit and should not be animated as a live
   * one, from being mistaken for this.
   */
  const liveRunId: Ref<string | null> = ref(null)

  function clearIdle() {
    pending.value = false
    if (idleId !== null) {
      clearTimeout(idleId)
      idleId = null
    }
  }

  function clearWatchdog() {
    if (watchdogId !== null) {
      clearTimeout(watchdogId)
      watchdogId = null
    }
  }

  /**
   * Stops the live run identified by `id`, if it is still the one running.
   *
   * Checking the id is what keeps the watchdog from reaching past its own run:
   * by the time it fires the live run may be long over and a manual one --
   * which has no time limit -- may be in flight in its place.
   */
  function watchdogFired(id: string) {
    watchdogId = null
    if (hooks.currentRunId() !== id) return
    hooks.stopRun()
    hooks.reportTimeout(runLimitMs)
  }

  /**
   * Runs the file now rather than waiting out the idle delay: for a file that
   * has just been opened, whose output would otherwise sit empty until the
   * first keystroke.
   *
   * Self-guarding -- it does nothing where a scheduled run would not have
   * happened either -- and it drops any run already pending, since this one
   * supersedes it.
   */
  async function runNow() {
    clearIdle()
    // Re-checked here, not just when scheduling: the delay is long enough for
    // the file to have been closed, or a step to have started, in between.
    if (!liveEvaluation.value || !hooks.canRun()) return
    const started = generation
    await hooks.run()
    // Cancelled while the run was being scheduled -- the file was switched, or
    // the feature turned off. Claiming it now would undo that, and the
    // watchdog would later stop it in live evaluation's name.
    if (started !== generation) return
    const id = hooks.currentRunId()
    liveRunId.value = id
    // Nothing to watch: the program did not compile, or it finished as it was
    // scheduled (a file of nothing but `define`s usually has).
    if (id === null) return
    clearWatchdog()
    watchdogId = setTimeout(() => {
      watchdogFired(id)
    }, runLimitMs)
  }

  /**
   * Notes that the document changed. Restarts the idle timer, or cancels a
   * pending run where live evaluation is off or not currently allowed.
   */
  function noteEdit(): void {
    clearIdle()
    if (!liveEvaluation.value || !hooks.canRun()) return
    pending.value = true
    idleId = setTimeout(() => {
      void runNow()
    }, idleMs)
  }

  /**
   * Drops a pending run and disarms the watchdog, without touching whatever is
   * running. For a file switch, a close, or the feature being turned off: the
   * scheduled run is about the document that is going away.
   */
  function cancel(): void {
    generation += 1
    clearIdle()
    clearWatchdog()
    // The run it named is no longer live evaluation's to manage, whether or
    // not it is still going, so the control should stop presenting it as one.
    liveRunId.value = null
  }

  // Guarded so the composable can be exercised without a component around it;
  // in the IDE this is what stops a timer outliving the page.
  if (getCurrentScope()) {
    onScopeDispose(cancel)
  }

  return { noteEdit, runNow, cancel, pending, liveRunId }
}

export type LiveEvaluation = ReturnType<typeof useLiveEvaluation>
