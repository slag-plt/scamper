<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from 'vue'
import { EditorView } from '@codemirror/view'
import { setDiagnostics } from '@codemirror/lint'
import type { Diagnostic } from '@codemirror/lint'
import {
  mkCellEditorState,
  type CellChange,
  type CellEditorHandle,
} from '../codemirror/cell-editor'
import { setLspContext } from '../codemirror/lsp'
import {
  completionCompartment,
  completionExtension,
  editorThemeCompartment,
  editorThemeExtension,
  fontSizeCompartment,
  fontSizeExtension,
} from '../codemirror/codemirror'
import { currentTheme } from '../../../theme'
import { autoSuggest, editorFontSize } from '../editor-prefs'

/**
 * One cell of Scamper source: the box a REPL entry is typed into, and the
 * record of one already run (#399).
 *
 * A real editor rather than highlighted text, even where it is read-only, so
 * that an entry reads and behaves the same before and after it is run -- and
 * so a notebook, whose cells are editable after the fact, has the component it
 * needs.
 */
const props = defineProps<{
  /** The cell's initial text. A live cell is cleared through `clear`. */
  source?: string
  isReadOnly?: boolean
  /**
   * The document URI this cell holds, which turns the language server on for
   * it. One per live cell; omitted for a cell that is only a record.
   */
  lspUri?: string
  /**
   * The source this cell continues, for the language server to analyse it
   * inside: a name defined out there is in scope in here. Ignored without
   * `lspUri`.
   */
  context?: string
  /** What the cell is written in; Scamper unless it is a notebook's prose. */
  language?: 'scamper' | 'markdown'
  /**
   * Whether Enter runs the cell rather than adding a line to it.
   *
   * True for the REPL's prompt, where an entry is one form and Enter is how it
   * is run (#399). False everywhere else: a notebook cell is never run on its
   * own -- the whole program is -- so Enter there is an ordinary newline, and
   * binding it to a `submit` nobody listens for would swallow it instead
   * (#410).
   */
  runOnEnter?: boolean
}>()

const emit = defineEmits<{
  submit: [text: string]
  /** The caret tried to leave the top (-1) or bottom (1) of the cell. */
  history: [direction: -1 | 1, handled: { value: boolean }]
  /** What the person changed, for a cell that is a view of a document. */
  change: [changes: CellChange[]]
  focusChange: [focused: boolean]
  /** Where the caret is in this cell, so a notebook can follow it. */
  cursor: [pos: number]
}>()

/**
 * True while the cell is being written into from outside, so the edit that
 * puts it there is not reported back as one the person made -- which for a
 * notebook would be an edit chasing its own tail.
 */
let applying = false

const containerRef = ref<HTMLDivElement | null>(null)
let view: EditorView | null = null

onMounted(() => {
  if (containerRef.value === null) return
  // Before the editor exists, so the first analysis -- which the client asks
  // for as it opens the document -- already knows what this cell continues.
  if (props.lspUri !== undefined) {
    setLspContext(props.lspUri, props.context ?? '')
  }
  view = new EditorView({
    state: mkCellEditorState(props.source ?? '', {
      isReadOnly: props.isReadOnly,
      lspUri: props.lspUri,
      language: props.language,
      // Left unset unless Enter is meant to run the cell: the binding swallows
      // Enter for anything that already parses, which in a cell nobody submits
      // means no newline at all.
      onSubmit:
        props.runOnEnter
          ? (text) => {
              emit('submit', text)
            }
          : undefined,
      onChange: (changes) => {
        if (!applying) emit('change', changes)
      },
      onFocusChange: (focused) => {
        emit('focusChange', focused)
      },
      onCursor: (pos) => {
        emit('cursor', pos)
      },
      // Vue events cannot return a value, so the listener reports through the
      // box: unhandled means CodeMirror moves the caret as it normally would.
      onHistory: (direction) => {
        const handled = { value: false }
        emit('history', direction, handled)
        return handled.value
      },
    }),
    parent: containerRef.value,
  })
})

onUnmounted(() => {
  view?.destroy()
  view = null
})

// Each entry that runs adds to what the next one continues.
watch(
  () => props.context,
  (context) => {
    if (props.lspUri !== undefined) setLspContext(props.lspUri, context ?? '')
  },
)

// As the main editor does, so a cell is not left in last session's colours or
// last minute's font size.
watch(currentTheme, (theme) => {
  view?.dispatch({
    effects: editorThemeCompartment.reconfigure(editorThemeExtension(theme)),
  })
})

watch(editorFontSize, (px) => {
  view?.dispatch({
    effects: fontSizeCompartment.reconfigure(fontSizeExtension(px)),
  })
})

// A cell without the language server -- a prose cell, or one already run --
// has no completion compartment. Reconfiguring one the state does not hold
// leaves it unchanged, so this needs no guard of its own.
watch(autoSuggest, (on) => {
  view?.dispatch({
    effects: completionCompartment.reconfigure(completionExtension(on)),
  })
})

/** Replaces the cell's contents, leaving the caret at the end. */
function setText(text: string): void {
  if (view === null) return
  applying = true
  try {
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: text },
      selection: { anchor: text.length },
    })
  } finally {
    applying = false
  }
}

defineExpose<CellEditorHandle>({
  setText,
  clear: () => {
    setText('')
  },
  focus: (at?: 'start' | 'end' | number) => {
    if (view === null) return
    if (at !== undefined) {
      const end = view.state.doc.length
      view.dispatch({
        selection: {
          anchor:
            at === 'start'
              ? 0
              : at === 'end'
                ? end
                : // An offset, from a notebook putting the caret back where a
                  // re-split moved it from. Clamped: the cell it lands in may
                  // be shorter than the one it left.
                  Math.max(0, Math.min(at, end)),
        },
      })
    }
    view.focus()
  },
  text: () => view?.state.doc.toString() ?? '',
  // The lint extension comes with the effect, so a cell that has never had a
  // diagnostic needs nothing configured for its first one.
  setDiagnostics: (diagnostics: Diagnostic[]) => {
    if (view !== null) view.dispatch(setDiagnostics(view.state, diagnostics))
  },
})
</script>

<template>
  <div ref="containerRef" class="cell-editor"></div>
</template>

<style scoped>
.cell-editor {
  min-width: 0;
}

/* The cell is one form, so it sizes to its contents rather than scrolling
   inside a box of its own -- the transcript around it is what scrolls. */
.cell-editor :deep(.cm-editor) {
  background: transparent;
}

.cell-editor :deep(.cm-scroller) {
  overflow: visible;
  font-family: var(--font-mono, monospace);
}

.cell-editor :deep(.cm-content) {
  padding: 0;
}

.cell-editor :deep(.cm-line) {
  padding-left: 0;
}

/* A cell that has been run has no caret to show. */
.cell-editor :deep(.cm-editor:not(.cm-focused) .cm-cursor) {
  display: none;
}

/*
 * The `;` against every line of a prose cell (#410).
 *
 * Coloured as a comment and set apart from the text, because that is what it
 * is: the marker the file holds, shown so that writing a paragraph in a
 * notebook still reads as writing a comment in a Scheme file. It lives in the
 * gutter, so it can be read and copied but never typed over.
 */
.cell-editor :deep(.cm-comment-gutter) {
  background: transparent;
  color: var(--syntax-comment);
  font-family: var(--font-mono, monospace);
  user-select: none;
}

.cell-editor :deep(.cm-comment-gutter .cm-gutterElement) {
  padding: 0 0.6em 0 0;
}

/* The gutter is the only thing between the cell's edge and its text. */
.cell-editor :deep(.cm-gutters) {
  background: transparent;
  border: none;
}

/* What an empty cell says: plainly a prompt rather than something written. */
.cell-editor :deep(.cm-placeholder) {
  color: var(--syntax-comment);
  font-style: italic;
  opacity: 0.75;
}
</style>
