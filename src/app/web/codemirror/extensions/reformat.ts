import { Extension } from '@codemirror/state'
import { Command, keymap } from '@codemirror/view'
import { diff } from '@codemirror/merge'
import { formatSource } from '../../../../scheme/format'
import { PRINT_WIDTH } from '../../../../scheme/style'
import { formatMode } from '../../editor-prefs'

/**
 * Reformats the whole document, preserving the selection.
 *
 * This re-flows the file -- it decides where the line breaks go -- where Ctrl-I
 * only re-indents the lines the user already has. Both read their rules from
 * src/scheme/style.ts, so the two never disagree about where a form belongs.
 *
 * How much breaking to do is the person's choice (Edit > Relaxed Formatting),
 * and the panes read the same setting, so a file and a trace are laid out
 * alike.
 */
export const formatScamperDocument: Command = (view) => {
  const oldText = view.state.doc.toString()
  let formatted: string
  try {
    formatted = formatSource(oldText, PRINT_WIDTH, formatMode.value)
  } catch {
    // Mid-edit the document is often unparseable; leave it alone.
    return true
  }

  // Normalize line endings to match CodeMirror's internal model.
  const newText = formatted.replace(/\r\n?/g, '\n')
  if (newText === oldText) {
    return true
  }

  const sel = view.state.selection.main
  // Compute a minimal edit script so selection positions can be mapped.
  const chunks = diff(oldText, newText)
  const changes = chunks.map((c) => ({
    from: c.fromA,
    to: c.toA,
    insert: newText.slice(c.fromB, c.toB),
  }))
  const changeSet = view.state.changes(changes)

  view.dispatch({
    changes,
    selection: {
      anchor: changeSet.mapPos(sel.anchor, 1),
      head: changeSet.mapPos(sel.head, 1),
    },
    scrollIntoView: true,
  })
  return true
}

export const ReformatExtension: Extension = keymap.of([
  // Prefer Mod on macOS (Cmd) and Ctrl elsewhere
  { key: 'Mod-Shift-i', run: formatScamperDocument },
])
