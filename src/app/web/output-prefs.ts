import { ref } from 'vue'

/**
 * How the output pane is displayed.
 *
 * Module-level and self-persisting, like src/theme and editor-prefs: the
 * toggle is offered in two places (the View menu and the output window's own
 * toolbar) and read in a third (the output pane), so routing it through props
 * would mean threading one boolean the length of the component tree.
 */

const SHOW_SOURCE_KEY = 'scamper.output.showSource'

/**
 * Whether each run of output is captioned with the statement that produced it.
 *
 * The source is always emitted into the output; this only decides whether it is
 * shown. That keeps toggling it instant and retroactive -- output already on
 * screen gains its captions rather than only the next run having them.
 */
export const showSourceWithOutput = ref<boolean>(
  (() => {
    try {
      // On unless turned off. A column of bare values with nothing tying each
      // one to the statement that produced it is hard to read the moment a
      // program prints more than a couple of things, and the captions are also
      // what separate one result from the next.
      return localStorage.getItem(SHOW_SOURCE_KEY) !== 'false'
    } catch {
      return true // no storage; default to on
    }
  })(),
)

export function setShowSourceWithOutput(on: boolean): void {
  showSourceWithOutput.value = on
  try {
    localStorage.setItem(SHOW_SOURCE_KEY, String(on))
  } catch {
    // Applies for this session regardless; remembering it is a bonus.
  }
}

export function toggleShowSourceWithOutput(): void {
  setShowSourceWithOutput(!showSourceWithOutput.value)
}
