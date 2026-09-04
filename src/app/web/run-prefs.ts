import { ref } from 'vue'
import { DEFAULT_TRACE_STEP_LIMIT } from '../../lpm/output/trace-collector'
import {
  DEFAULT_MAX_CALL_STACK_DEPTH,
  MAX_CALL_STACK_DEPTH,
  MIN_CALL_STACK_DEPTH,
  setDefaultMaxCallStackDepth,
} from '../../lpm/limits'

/**
 * How and when the current file is run.
 *
 * Module-level and self-persisting, like editor-prefs and output-prefs: the
 * toggles are set from the Run menu and the preferences pane and read where
 * edits are noticed, so routing them through props would mean threading one
 * boolean down the component tree.
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

/**
 * Reads a number this module has stored, held to `clamp`.
 *
 * @returns `fallback` when nothing is stored, when what is stored is not a
 *          number, or when there is no storage to read at all.
 */
function storedNumber(
  key: string,
  fallback: number,
  clamp: (value: number) => number,
): number {
  try {
    const stored = localStorage.getItem(key)
    if (stored === null) return fallback
    const value = Number(stored)
    return Number.isFinite(value) ? clamp(value) : fallback
  } catch {
    return fallback // no storage; default as if unset
  }
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
 * run, and this module is already what the preferences pane reads for its
 * Running section.
 */
export const traceStepLimit = ref<number>(
  storedNumber(
    TRACE_STEP_LIMIT_KEY,
    DEFAULT_TRACE_STEP_LIMIT,
    clampTraceStepLimit,
  ),
)

export function setTraceStepLimit(steps: number): void {
  traceStepLimit.value = clampTraceStepLimit(steps)
  try {
    localStorage.setItem(TRACE_STEP_LIMIT_KEY, String(traceStepLimit.value))
  } catch {
    // Applies for this session regardless; remembering it is a bonus.
  }
}

const MAX_RECURSION_DEPTH_KEY = 'scamper.run.recursiondepth'

/** The range a recursion depth may be chosen from; see lpm/limits.ts. */
export const MIN_RECURSION_DEPTH = MIN_CALL_STACK_DEPTH
export const MAX_RECURSION_DEPTH = MAX_CALL_STACK_DEPTH

function clampRecursionDepth(depth: number): number {
  return Math.min(
    MAX_RECURSION_DEPTH,
    Math.max(MIN_RECURSION_DEPTH, Math.round(depth)),
  )
}

/**
 * How deep a recursion may go before the machine reports `Max call stack depth
 * exceeded` (issue #477).
 *
 * The depth a *fresh fiber starts at* rather than the last word on the matter:
 * a program that calls `set-maximum-recursion-depth!` still overrides it for
 * its own run, so what a program does stays reproducible and this is only the
 * floor a student sets for the session. Before the preferences pane that call
 * was the only way to ask at all, and the next Run undid it.
 */
export const maxRecursionDepth = ref<number>(
  storedNumber(
    MAX_RECURSION_DEPTH_KEY,
    DEFAULT_MAX_CALL_STACK_DEPTH,
    clampRecursionDepth,
  ),
)

export function setMaxRecursionDepth(depth: number): void {
  maxRecursionDepth.value = clampRecursionDepth(depth)
  setDefaultMaxCallStackDepth(maxRecursionDepth.value)
  try {
    localStorage.setItem(
      MAX_RECURSION_DEPTH_KEY,
      String(maxRecursionDepth.value),
    )
  } catch {
    // Applies for this session regardless; remembering it is a bonus.
  }
}

// Handed to the machine as this module loads rather than when the pane is first
// opened, so the two agree before anything is run: the pane is not necessarily
// opened at all in a session that has one of these stored from an earlier one.
setDefaultMaxCallStackDepth(maxRecursionDepth.value)
