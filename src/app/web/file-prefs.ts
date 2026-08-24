import { ref } from 'vue'

/**
 * What the file drawer shows.
 *
 * Module-level and self-persisting, like editor-prefs and output-prefs: the
 * toggle is offered in the View menu and read where the drawer is populated,
 * so routing it through props would mean threading one boolean down the
 * component tree.
 */

const SHOW_HIDDEN_KEY = 'scamper.files.showHidden'

/**
 * Whether the drawer lists internal files -- a dotted name marks something an
 * app keeps for itself, such as a file's saved history (see isHiddenName).
 *
 * Off unless turned on. These are Scamper's own bookkeeping rather than the
 * student's work, and a drawer that mixes the two invites opening, renaming or
 * deleting one by accident. On, it is a way to see what is actually there
 * (issue #178), which is mostly useful when debugging.
 */
export const showHiddenFiles = ref<boolean>(
  (() => {
    try {
      return localStorage.getItem(SHOW_HIDDEN_KEY) === 'true'
    } catch {
      return false // no storage; default to off
    }
  })(),
)

export function setShowHiddenFiles(on: boolean): void {
  showHiddenFiles.value = on
  try {
    localStorage.setItem(SHOW_HIDDEN_KEY, String(on))
  } catch {
    // Applies for this session regardless; remembering it is a bonus.
  }
}

export function toggleShowHiddenFiles(): void {
  setShowHiddenFiles(!showHiddenFiles.value)
}
