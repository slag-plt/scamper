import { ref } from 'vue'

/**
 * Which view of the open file the IDE shows (#410).
 *
 * Two ways of looking at the same file, not two documents:
 *
 * - `source` -- the file as text, with its output beside it. What Scamper has
 *   always shown.
 * - `notebook` -- the file as its forms, each with what it printed underneath,
 *   and the comments between them as prose. The output pane goes away, because
 *   the notebook *is* the output.
 *
 * Module-level and self-persisting, as output-prefs and editor-prefs are: the
 * choice is offered in the View menu and read by the IDE shell, and someone who
 * works in the notebook should still be in it tomorrow.
 */

export type FileView = 'source' | 'notebook'

const VIEW_KEY = 'scamper.view'

export const fileView = ref<FileView>(
  (() => {
    try {
      return localStorage.getItem(VIEW_KEY) === 'notebook'
        ? 'notebook'
        : 'source'
    } catch {
      return 'source' // no storage; the view Scamper has always opened in
    }
  })(),
)

export function setFileView(view: FileView): void {
  fileView.value = view
  try {
    localStorage.setItem(VIEW_KEY, view)
  } catch {
    // Applies for this session regardless; remembering it is a bonus.
  }
}

export function toggleFileView(): void {
  setFileView(fileView.value === 'notebook' ? 'source' : 'notebook')
}
