<script setup lang="ts">
import { nextTick, ref, watch } from 'vue'
import type { Diagnostic } from '@codemirror/lint'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'
import CellEditor from './CellEditor.vue'
import type { Value } from '../../../lpm'
import type { CellChange, CellEditorHandle } from '../codemirror/cell-editor'
import type { NotebookCell } from '../composables/use-notebook'
import { markdownToProse, proseToMarkdown } from '../notebook-cells'
import { renderMarkdown } from '../markdown'

/**
 * One cell of the notebook: a form and what it printed, or a paragraph of the
 * file's comments shown as Markdown (#410).
 *
 * The editor inside is the same one the REPL types into, so a cell reads and
 * behaves like the rest of the IDE. What is different is that this one is a
 * view of a stretch of the open file: every edit is reported upward and written
 * through, and nothing is kept here that the file does not already hold.
 */
const props = defineProps<{
  cell: NotebookCell
  /** What this cell printed when the program last ran. */
  output: Value[]
  /** What is wrong in it, in the cell's own coordinates. */
  diagnostics: Diagnostic[]
  /**
   * The file up to this cell, for the language server: a name defined above is
   * in scope here. Only the cell being typed in has one -- it is the only one
   * that can ask for a completion.
   */
  context?: string
  /** Its position, which is all the notebook needs to say which cell it is. */
  index: number
}>()

const emit = defineEmits<{
  change: [changes: CellChange[]]
  /** A prose cell, rewritten whole: its Markdown is not the file's text. */
  replace: [text: string]
  focusChange: [focused: boolean]
  /** Where the caret is in this cell, in the cell's own coordinates. */
  cursor: [pos: number]
  /** The caret left the top (-1) or the bottom (1) of the cell. */
  move: [direction: -1 | 1]
  remove: []
}>()

const editorRef = ref<CellEditorHandle | null>(null)
const proseRef = ref<HTMLDivElement | null>(null)
/**
 * True while a prose cell is being written rather than read.
 *
 * A cell someone has just asked for starts open: it holds nothing, so there is
 * nothing to read, and being asked to click an empty cell to write in the empty
 * cell you asked for is a step with no purpose.
 */
const isEditing = ref(props.cell.kind === 'prose' && props.cell.isDraft === true)
const isFocused = ref(false)

/** One document each, so the language server can hold them all at once. */
const lspUri = `inmemory://notebook-${String(props.cell.id)}.scm`

/**
 * The file's own text is what a code cell holds; a prose cell holds the
 * Markdown inside its comments.
 */
function sourceOf(): string {
  return props.cell.kind === 'code'
    ? props.cell.text
    : proseToMarkdown(props.cell.text)
}

// Rendered rather than bound with v-html: nothing in a student's file may
// become markup (see markdown.ts).
watch(
  [() => props.cell.text, isEditing],
  () => {
    if (props.cell.kind !== 'prose' || isEditing.value) return
    void nextTick(() => {
      const el = proseRef.value
      if (el === null) return
      const markdown = proseToMarkdown(props.cell.text)
      // A cell with nothing in it renders to nothing, which would leave no
      // sign that it is there and nothing to click to write in it. It says so
      // instead.
      el.replaceChildren(
        markdown.trim().length === 0 ? placeholder() : renderMarkdown(markdown),
      )
    })
  },
  { immediate: true },
)

function placeholder(): HTMLElement {
  const hint = document.createElement('span')
  hint.className = 'prose-placeholder'
  hint.textContent = 'Empty text cell. Click to write something.'
  return hint
}

// The document changed underneath: a file was opened, an edit was undone, the
// file was reformatted. Never while the caret is here, since then the text in
// the cell is what the person is in the middle of typing.
watch(
  () => props.cell.text,
  (text) => {
    if (props.cell.kind !== 'code' || isFocused.value) return
    if (editorRef.value !== null && editorRef.value.text() !== text) {
      editorRef.value.setText(text)
    }
  },
)

watch(
  () => props.diagnostics,
  (diagnostics) => {
    editorRef.value?.setDiagnostics(diagnostics)
  },
  { immediate: true },
)

function onFocusChange(focused: boolean) {
  isFocused.value = focused
  emit('focusChange', focused)
  if (!focused && isEditing.value) stopEditingProse()
}

function onChange(changes: CellChange[]) {
  // A prose cell is written back whole rather than by the edit that changed
  // it: its Markdown and the file's comment lines are not the same text, so an
  // edit in one is not an edit in the other.
  //
  // As it is typed, though, not when the caret leaves. The file is what gets
  // saved, run and kept in the history, so text that is only in a cell is text
  // that is not really anywhere yet.
  if (props.cell.kind === 'prose') {
    emit('replace', markdownToProse(editorRef.value?.text() ?? ''))
    return
  }
  emit('change', changes)
}

function editProse() {
  isEditing.value = true
  void nextTick(() => {
    editorRef.value?.focus('end')
  })
}

/**
 * Goes back to showing the prose rather than the Markdown behind it.
 *
 * Nothing is written here: what was typed reached the file as it was typed
 * (see `onChange`).
 */
function stopEditingProse() {
  isEditing.value = false
}

/** Up and down at the edges of a cell move to the next one, as a caret does. */
function onHistory(direction: -1 | 1, handled: { value: boolean }) {
  handled.value = true
  emit('move', direction)
}

defineExpose({
  focus: (at?: 'start' | 'end') => {
    if (props.cell.kind === 'prose' && !isEditing.value) {
      editProse()
      return
    }
    editorRef.value?.focus(at)
  },
})
</script>

<template>
  <div class="notebook-cell" :class="`notebook-cell-${cell.kind}`">
    <div class="cell-body">
      <!-- A prose cell is read until it is clicked into, so a lab handout
           reads as a handout rather than as a wall of comment markers. -->
      <div
        v-if="cell.kind === 'prose' && !isEditing"
        ref="proseRef"
        class="notebook-prose"
        role="button"
        tabindex="0"
        title="Click to edit this text"
        @click="editProse"
        @keydown.enter.prevent="editProse"
      ></div>
      <CellEditor
        v-else
        ref="editorRef"
        :source="sourceOf()"
        :language="cell.kind === 'prose' ? 'markdown' : 'scamper'"
        :lsp-uri="cell.kind === 'code' ? lspUri : undefined"
        :context="context"
        @change="onChange"
        @focus-change="onFocusChange"
        @cursor="(pos) => { emit('cursor', pos) }"
        @history="onHistory"
      />
      <button
        type="button"
        class="cell-remove"
        title="Remove this cell"
        aria-label="Remove this cell"
        @click="emit('remove')"
      >
        <i class="fa-solid fa-xmark" aria-hidden="true"></i>
      </button>
    </div>

    <!-- Under the code that produced it, which is the whole point of the
         view. A cell that printed nothing shows nothing. -->
    <div v-if="output.length > 0" class="cell-output" role="log">
      <div v-for="(value, i) in output" :key="i" class="cell-value">
        <ValueRenderer :value="value" />
      </div>
    </div>
  </div>
</template>

<style scoped>
.notebook-cell {
  display: flex;
  flex-direction: column;
}

.cell-body {
  position: relative;
  display: flex;
  align-items: flex-start;
  gap: var(--space-xs);
  border-left: 3px solid transparent;
  padding: var(--space-xs) var(--space-sm);
  border-radius: var(--radius-sm);
}

/* Ruled down its left edge like the output pane's source captions, so a cell
   reads as code that ran rather than as a box to fill in. */
.notebook-cell-code .cell-body {
  border-left-color: var(--border);
  background: var(--surface-muted);
}

.notebook-cell-code .cell-body:focus-within {
  border-left-color: var(--brand);
}

.cell-body > :deep(*:first-child) {
  flex: 1;
  min-width: 0;
}

.cell-remove {
  flex: 0 0 auto;
  visibility: hidden;
  border: none;
  background: none;
  padding: 0 var(--space-xs);
  color: inherit;
  opacity: 0.6;
  cursor: pointer;
}

.cell-body:hover .cell-remove,
.cell-body:focus-within .cell-remove {
  visibility: visible;
}

.cell-remove:hover {
  opacity: 1;
}

.cell-output {
  padding: var(--space-xs) var(--space-sm) var(--space-sm);
  white-space: pre-wrap;
}

/* A line's worth of height whatever is in it, so a cell holding nothing --
   or a comment that renders to nothing -- is still there to be clicked. */
.notebook-prose {
  cursor: text;
  min-height: 1.5em;
}

/* The same treatment the editor's own placeholder gets, so the cell reads as
   a prompt in both states rather than as something someone wrote. */
.prose-placeholder {
  color: var(--syntax-comment);
  font-style: italic;
  opacity: 0.75;
}

.notebook-prose:focus-visible {
  outline: 2px solid var(--brand);
  outline-offset: 2px;
}

/* Markdown, as a page of prose rather than as source. */
.notebook-prose :deep(h1),
.notebook-prose :deep(h2),
.notebook-prose :deep(h3),
.notebook-prose :deep(h4) {
  margin: 0.4em 0 0.2em;
  line-height: 1.2;
}

.notebook-prose :deep(p) {
  margin: 0.4em 0;
}

.notebook-prose :deep(ul),
.notebook-prose :deep(ol) {
  margin: 0.4em 0;
  padding-left: 1.5em;
}

.notebook-prose :deep(code) {
  font-family: var(--font-mono, monospace);
  font-size: 0.9em;
  background: var(--surface-muted);
  border-radius: var(--radius-sm);
  padding: 0 0.2em;
}

.notebook-prose :deep(pre) {
  margin: 0.4em 0;
  padding: var(--space-xs) var(--space-sm);
  background: var(--surface-muted);
  border-radius: var(--radius-sm);
  overflow-x: auto;
}

.notebook-prose :deep(pre) code {
  background: none;
  padding: 0;
}

.notebook-prose :deep(blockquote) {
  margin: 0.4em 0;
  padding-left: var(--space-sm);
  border-left: 3px solid var(--border);
  opacity: 0.85;
}

.notebook-prose :deep(img) {
  max-width: 100%;
}
</style>
