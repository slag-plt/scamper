import { Extension } from '@codemirror/state'
import { keymap } from '@codemirror/view'
import { format } from 'prettier'
import ScamperPlugin from '../../../prettier/prettier-plugin-scamper'
import { diff } from '@codemirror/merge'

export const PrettierExtension: Extension = keymap.of([
  {
    // Prefer Mod on macOS (Cmd) and Ctrl elsewhere
    key: 'Mod-Shift-i',
    run: (view) => {
      const doc = view.state.doc
      const sel = view.state.selection.main
      const oldAnchor = sel.anchor
      const oldHead = sel.head
      const oldText = doc.toString()

      void format(oldText, {
        parser: 'scamper-scheme',
        plugins: [ScamperPlugin],
      })
        .then((formatted) => {
          // Normalize line endings to match CodeMirror's internal model
          const newText = formatted.replace(/\r\n?/g, '\n')
          if (newText === oldText) {
            return
          }

          // Compute a minimal edit script so selection positions can be mapped.
          const chunks = diff(oldText, newText)
          const changes = chunks.map((c) => ({
            from: c.fromA,
            to: c.toA,
            insert: newText.slice(c.fromB, c.toB),
          }))
          const changeSet = view.state.changes(changes)
          const newAnchor = changeSet.mapPos(oldAnchor, 1)
          const newHead = changeSet.mapPos(oldHead, 1)

          view.dispatch({
            changes,
            selection: { anchor: newAnchor, head: newHead },
            scrollIntoView: true,
          })
        })
        .catch(() => {
          // If formatting fails, do nothing; keep editor state unchanged.
        })

      return true
    },
  },
])
