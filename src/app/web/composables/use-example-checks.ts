import { getCurrentScope, onScopeDispose, shallowRef, type ShallowRef } from 'vue'
import Scamper from '../../../scamper'
import { compileExamples, type CompiledExample } from '../../../scheme'
import { classifyExampleRun, type ExampleOutcome } from '../../../scheme/examples'
import { checkExamples } from '../run-prefs'

/**
 * Checking `@example` lines (issue #374).
 *
 * An example is checked by running the whole file again with the example's
 * call reported at the end, so each one costs a run. That is why checks happen
 * when a run *finishes* rather than as the file is typed: live evaluation
 * (#378) already waits for the typing to stop, so this adds no timer of its
 * own and inherits that policy.
 *
 * Two things keep the cost bounded:
 *
 * - Checks run one at a time, so ten examples are ten runs in sequence rather
 *   than ten fibers at once.
 * - Each gets a watchdog. There is no step budget in the scheduler, so an
 *   example that loops forever is stopped from outside after
 *   {@link EXAMPLE_LIMIT_MS} -- shorter than live evaluation's own limit,
 *   since an example is meant to be a quick check.
 *
 * Framework-light on purpose, like use-live-evaluation.ts: it is a plain
 * object of functions, testable without a mounted component.
 */

/** How long one example may run before the watchdog stops it. */
export const EXAMPLE_LIMIT_MS = 2000

export interface ExampleChecksOptions {
  limitMs?: number
}

export function useExampleChecks(options: ExampleChecksOptions = {}) {
  const limitMs = options.limitMs ?? EXAMPLE_LIMIT_MS

  /** What the last sweep found, one entry per `@example` line checked. */
  const outcomes: ShallowRef<readonly ExampleOutcome[]> = shallowRef([])

  /**
   * Bumped by {@link cancel}, so a sweep still running can tell that its
   * findings are about a document that is no longer open.
   */
  let generation = 0

  /**
   * Runs one example, giving up on it after `limitMs`.
   *
   * A cancelled run never settles -- the scheduler drops the task without
   * completing it -- so the timeout is raced against it rather than awaited
   * after it.
   */
  async function runOne(example: CompiledExample): Promise<ExampleOutcome> {
    const scamper = Scamper.getInstance()
    const { id, done } = scamper.checkExample(example.prog)
    let watchdogId: ReturnType<typeof setTimeout> | undefined
    const timedOut = new Promise<'timeout'>((resolve) => {
      watchdogId = setTimeout(() => {
        scamper.cancel(id)
        resolve('timeout')
      }, limitMs)
    })
    const reported = await Promise.race([done, timedOut])
    clearTimeout(watchdogId)
    return reported === 'timeout'
      ? { range: example.range, status: 'timeout' }
      : { range: example.range, ...classifyExampleRun(reported) }
  }

  /**
   * Checks every `@example` in `src`, publishing each outcome as it lands so
   * the first mark appears without waiting for the last.
   *
   * A no-op beyond clearing the marks when the preference is off, or when the
   * file has no examples.
   */
  async function runChecks(src: string): Promise<void> {
    const started = ++generation
    outcomes.value = []
    if (!checkExamples.value) return
    const { examples } = compileExamples(src)
    const found: ExampleOutcome[] = []
    for (const example of examples) {
      const outcome = await runOne(example)
      // Superseded while that example ran: these findings are about a document
      // that has been replaced, so publishing them would mark the wrong lines.
      if (started !== generation) return
      found.push(outcome)
      outcomes.value = [...found]
    }
  }

  /** Drops the marks and abandons a sweep in flight: for a file switch or close. */
  function cancel(): void {
    generation += 1
    outcomes.value = []
  }

  // Guarded so the composable can be exercised without a component around it.
  if (getCurrentScope()) {
    onScopeDispose(cancel)
  }

  return { outcomes, runChecks, cancel }
}

export type ExampleChecks = ReturnType<typeof useExampleChecks>
