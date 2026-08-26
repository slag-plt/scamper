<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from 'vue'
import { EditorView } from '@codemirror/view'
import {
  mkCellEditorState,
  type CellEditorHandle,
} from '../codemirror/cell-editor'
import { setLspContext } from '../codemirror/lsp'
import {
  editorThemeCompartment,
  editorThemeExtension,
  fontSizeCompartment,
  fontSizeExtension,
} from '../codemirror/codemirror'
import { currentTheme } from '../../../theme'
import { editorFontSize } from '../editor-prefs'

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
}>()

const emit = defineEmits<{
  submit: [text: string]
  /** The caret tried to leave the top (-1) or bottom (1) of the cell. */
  history: [direction: -1 | 1, handled: { value: boolean }]
}>()

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
      onSubmit: (text) => {
        emit('submit', text)
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

/** Replaces the cell's contents, leaving the caret at the end. */
function setText(text: string): void {
  if (view === null) return
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: text },
    selection: { anchor: text.length },
  })
}

defineExpose<CellEditorHandle>({
  setText,
  clear: () => {
    setText('')
  },
  focus: () => {
    view?.focus()
  },
  text: () => view?.state.doc.toString() ?? '',
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
</style>
