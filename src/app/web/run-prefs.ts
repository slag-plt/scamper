import { ref } from 'vue'
import { DEFAULT_TRACE_STEP_LIMIT } from '../../lpm/output/trace-collector'

/**
 * How and when the current file is run.
 *
 * Module-level and self-persisting, like editor-prefs and output-prefs: the
 * toggle is set from the Run menu and read where edits are noticed, so routing
 * it through props would mean threading one boolean down the component tree.
 */

const LIVE_EVALUATION_KEY = 'scamper.run.live'

/**
 * Whether the file is re-run by itself shortly after the user stops typing
 * (issue #378).
 *
 * On unless turned off. Seeing what the code does without having to ask is the
 * point of the feature; the off switch is for a program whose output is
 * expensive or noisy to produce -- one that plays a sound, say.
 */
export const liveEvaluation = ref<boolean>(
  (() => {
    try {
      return localStorage.getItem(LIVE_EVALUATION_KEY) !== 'false'
    } catch {
      return true // no storage; default to on
    }
  })(),
)

export function setLiveEvaluation(on: boolean): void {
  liveEvaluation.value = on
  try {
    localStorage.setItem(LIVE_EVALUATION_KEY, String(on))
  } catch {
    // Applies for this session regardless; remembering it is a bonus.
  }
}

export function toggleLiveEvaluation(): void {
  setLiveEvaluation(!liveEvaluation.value)
}

const CHECK_EXAMPLES_KEY = 'scamper.run.examples'

/**
 * Whether each `;;; @example ...` line is checked once the file has run
 * (issue #374), marking it with whether the code agrees with it.
 *
 * On unless turned off. Checking re-runs the file once per example, so the off
 * switch is for a program whose every run is expensive.
 */
export const checkExamples = ref<boolean>(
  (() => {
    try {
      return localStorage.getItem(CHECK_EXAMPLES_KEY) !== 'false'
    } catch {
      return true // no storage; default to on
    }
  })(),
)

export function setCheckExamples(on: boolean): void {
  checkExamples.value = on
  try {
    localStorage.setItem(CHECK_EXAMPLES_KEY, String(on))
  } catch {
    // Applies for this session regardless; remembering it is a bonus.
  }
}

export function toggleCheckExamples(): void {
  setCheckExamples(!checkExamples.value)
}

const TRACE_STEP_LIMIT_KEY = 'scamper.run.tracesteps'

/**
 * The range a trace step limit is held to. The floor keeps a limit of 0 -- a
 * trace with nothing in it -- from being typed in by accident; the ceiling is
 * on what can be *asked for*, not on what is sensible. On the runaway
 * {@link DEFAULT_TRACE_STEP_LIMIT} was measured against, collection takes 2s at
 * 2500 steps and 47s at 10,000, and runs out of memory at 20,000: a large limit
 * is a choice to wait -- or, high enough, to lose the page and come back with a
 * smaller one.
 */
export const MIN_TRACE_STEP_LIMIT = 10
export const MAX_TRACE_STEP_LIMIT = 100_000

function clampTraceStepLimit(steps: number): number {
  return Math.min(
    MAX_TRACE_STEP_LIMIT,
    Math.max(MIN_TRACE_STEP_LIMIT, Math.round(steps)),
  )
}

/**
 * How many reductions stepping a statement may take before it gives up and
 * says so (issue #369).
 *
 * Here rather than in a file of its own because it is part of how the file is
 * run, and this module is already what the Run menu reads. It is set from that
 * menu until there is a preferences pane to hold it.
 */
export const traceStepLimit = ref<number>(
  (() => {
    try {
      const stored = localStorage.getItem(TRACE_STEP_LIMIT_KEY)
      if (stored === null) return DEFAULT_TRACE_STEP_LIMIT
      const steps = Number(stored)
      return Number.isFinite(steps)
        ? clampTraceStepLimit(steps)
        : DEFAULT_TRACE_STEP_LIMIT
    } catch {
      return DEFAULT_TRACE_STEP_LIMIT // no storage; default as if unset
    }
  })(),
)

export function setTraceStepLimit(steps: number): void {
  traceStepLimit.value = clampTraceStepLimit(steps)
  try {
    localStorage.setItem(TRACE_STEP_LIMIT_KEY, String(traceStepLimit.value))
  } catch {
    // Applies for this session regardless; remembering it is a bonus.
  }
}
