import { ref } from 'vue'

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
