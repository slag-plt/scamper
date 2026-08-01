import {
  Decoration,
  DecorationSet,
  EditorView,
  ViewPlugin,
  ViewUpdate,
} from '@codemirror/view'
import { Extension, StateEffect, StateField } from '@codemirror/state'
import { LSPPlugin } from '@codemirror/lsp-client'
import type { DocumentHighlight } from 'vscode-languageserver-protocol'

// Highlights every occurrence of the identifier under the caret, coloring its
// binding site differently from its uses -- served by textDocument/
// documentHighlight (DocumentHighlightKind.Write = the binder, Read = a use).

interface Mark {
  from: number
  to: number
  cls: string
}

const setHighlights = StateEffect.define<readonly Mark[]>()

const highlightField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(deco, tr) {
    deco = deco.map(tr.changes)
    for (const e of tr.effects) {
      if (e.is(setHighlights)) {
        deco = Decoration.set(
          e.value.map((m) => Decoration.mark({ class: m.cls }).range(m.from, m.to)),
          true,
        )
      }
    }
    return deco
  },
  provide: (f) => EditorView.decorations.from(f),
})

/** Debounce (ms) so rapid caret movement doesn't fire a request per keystroke. */
const HIGHLIGHT_DELAY = 100

const highlightPlugin = ViewPlugin.fromClass(
  class {
    private timer: ReturnType<typeof setTimeout> | undefined
    // Bumped per request so a slow response from a stale caret is discarded.
    private token = 0

    constructor(private readonly view: EditorView) {
      this.schedule()
    }

    update(u: ViewUpdate) {
      if (u.selectionSet || u.docChanged) {
        this.schedule()
      }
    }

    private schedule() {
      clearTimeout(this.timer)
      this.timer = setTimeout(() => void this.compute(), HIGHLIGHT_DELAY)
    }

    private async compute() {
      const view = this.view
      const plugin = LSPPlugin.get(view)
      const sel = view.state.selection.main
      if (
        plugin === null ||
        !sel.empty ||
        !plugin.client.serverCapabilities?.documentHighlightProvider
      ) {
        this.clear()
        return
      }
      const docBefore = view.state.doc
      const myToken = ++this.token
      plugin.client.sync()
      let result: DocumentHighlight[] | null
      try {
        result = await plugin.client.request<
          { textDocument: { uri: string }; position: { line: number; character: number } },
          DocumentHighlight[] | null
        >('textDocument/documentHighlight', {
          textDocument: { uri: plugin.uri },
          position: plugin.toPosition(sel.head),
        })
      } catch {
        return
      }
      // Discard if the caret moved again or the document changed under us.
      if (myToken !== this.token || view.state.doc !== docBefore) {
        return
      }
      const marks: Mark[] = (result ?? [])
        .map((h) => ({
          from: plugin.fromPosition(h.range.start),
          to: plugin.fromPosition(h.range.end),
          cls: h.kind === 3 ? 'cm-lsp-binder' : 'cm-lsp-occurrence',
        }))
        .filter((m) => m.to > m.from)
        .sort((a, b) => a.from - b.from)
      view.dispatch({ effects: setHighlights.of(marks) })
    }

    private clear() {
      if ((this.view.state.field(highlightField, false)?.size ?? 0) > 0) {
        this.view.dispatch({ effects: setHighlights.of([]) })
      }
    }

    destroy() {
      clearTimeout(this.timer)
    }
  },
)

// Uses read as a neutral box; the binder stands out in the brand teal.
const highlightTheme = EditorView.baseTheme({
  '.cm-lsp-occurrence': {
    backgroundColor: 'rgba(128, 128, 128, 0.25)',
    borderRadius: '2px',
  },
  '.cm-lsp-binder': {
    backgroundColor: 'rgba(77, 189, 188, 0.4)',
    borderRadius: '2px',
  },
  '&dark .cm-lsp-occurrence': { backgroundColor: 'rgba(160, 160, 160, 0.22)' },
  '&dark .cm-lsp-binder': { backgroundColor: 'rgba(77, 189, 188, 0.33)' },
})

/** Editor extension that highlights occurrences of the identifier under the caret. */
export function occurrenceHighlighter(): Extension {
  return [highlightField, highlightPlugin, highlightTheme]
}
