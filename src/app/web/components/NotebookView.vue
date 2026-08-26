<script setup lang="ts">
import { computed, nextTick, ref } from 'vue'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'
import NotebookCell from './NotebookCell.vue'
import type { Notebook } from '../composables/use-notebook'
import type { CellChange } from '../codemirror/cell-editor'

/**
 * The notebook view of the open file (#410): its forms as editable cells, each
 * with what it printed underneath, and the comments between them as prose.
 *
 * The file is the notebook -- there is no notebook format, and nothing is
 * written into a `.scm` file to make one. Every cell here is a stretch of the
 * document the editor holds, so editing one is editing the file, and a run is
 * a run of the whole program, as pressing Run has always been.
 */
const props = defineProps<{ notebook: Notebook }>()

/** What each cell exposes: enough to put the caret in it. */
const cellRefs = ref<{ focus: (at?: 'start' | 'end') => void }[]>([])
/** Which cell the caret is in, or -1. Only that one gets an LSP context. */
const focused = ref(-1)

const cells = computed(() => props.notebook.cells.value)

/**
 * The program above a cell, for the language server to analyse it inside.
 *
 * Built from the cells rather than sliced out of the document because that is
 * exactly what it means: the forms this one comes after.
 */
function contextFor(index: number): string | undefined {
  if (index !== focused.value) return undefined
  return cells.value
    .slice(0, index)
    .map((cell) => cell.text)
    .join('\n')
}

function onChange(index: number, changes: CellChange[]) {
  props.notebook.applyChanges(index, changes)
}

function onReplace(index: number, text: string) {
  props.notebook.replaceCell(index, text)
}

function onFocusChange(index: number, isFocused: boolean) {
  if (isFocused) {
    focused.value = index
  } else if (focused.value === index) {
    focused.value = -1
  }
}

/** Moves the caret to the next cell up or down, as it moves between lines. */
function onMove(index: number, direction: -1 | 1) {
  const to = index + direction
  if (to < 0 || to >= cells.value.length) return
  cellRefs.value[to]?.focus(direction === -1 ? 'end' : 'start')
}

async function addCell(index: number, kind: 'code' | 'prose') {
  const position = props.notebook.insertCell(index, kind)
  await nextTick()
  cellRefs.value[position]?.focus('end')
}

function removeCell(index: number) {
  props.notebook.removeCell(index)
}
</script>

<template>
  <div class="notebook">
    <div class="notebook-scroll">
      <!-- An error with no cell to point at: a file that would not compile at
           all. Above everything, since it is about the whole file. -->
      <div v-if="notebook.unplaced.value.length > 0" class="notebook-unplaced">
        <div
          v-for="(value, i) in notebook.unplaced.value"
          :key="i"
          class="cell-value"
        >
          <ValueRenderer :value="value" />
        </div>
      </div>

      <p v-if="cells.length === 0" class="notebook-empty">
        This file is empty. Add a cell to start writing.
      </p>

      <template v-for="(cell, index) in cells" :key="cell.id">
        <NotebookCell
          ref="cellRefs"
          :cell="cell"
          :index="index"
          :output="notebook.outputOf(index)"
          :diagnostics="notebook.diagnostics.value[index] ?? []"
          :context="contextFor(index)"
          @change="(changes) => { onChange(index, changes) }"
          @replace="(text) => { onReplace(index, text) }"
          @focus-change="(isFocused) => { onFocusChange(index, isFocused) }"
          @cursor="(pos) => { notebook.noteCursor(index, pos) }"
          @move="(direction) => { onMove(index, direction) }"
          @remove="() => { removeCell(index) }"
        />
        <!-- Between one cell and the next, where a new one would go. Shown on
             hover so a page of cells is not a page of buttons. -->
        <div class="notebook-gap">
          <button
            type="button"
            class="gap-button"
            title="Add a code cell here"
            @click="() => void addCell(index, 'code')"
          >
            <i class="fa-solid fa-plus" aria-hidden="true"></i> Code
          </button>
          <button
            type="button"
            class="gap-button"
            title="Add a text cell here"
            @click="() => void addCell(index, 'prose')"
          >
            <i class="fa-solid fa-plus" aria-hidden="true"></i> Text
          </button>
        </div>
      </template>

      <div v-if="cells.length === 0" class="notebook-gap notebook-gap-open">
        <button
          type="button"
          class="gap-button"
          @click="() => void addCell(-1, 'code')"
        >
          <i class="fa-solid fa-plus" aria-hidden="true"></i> Code
        </button>
        <button
          type="button"
          class="gap-button"
          @click="() => void addCell(-1, 'prose')"
        >
          <i class="fa-solid fa-plus" aria-hidden="true"></i> Text
        </button>
      </div>
    </div>
  </div>
</template>

<style scoped>
.notebook {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.notebook-scroll {
  flex: 1;
  min-height: 0;
  overflow: auto;
  padding: var(--space-sm);
}

.notebook-unplaced {
  margin-bottom: var(--space-sm);
  white-space: pre-wrap;
}

.notebook-empty {
  margin: 0 0 var(--space-sm);
  opacity: 0.7;
  font-style: italic;
}

/* The seam between two cells. It keeps its height whether or not the buttons
   are showing, so the page does not jump as the pointer crosses it. */
.notebook-gap {
  display: flex;
  justify-content: center;
  gap: var(--space-xs);
  height: 1.6em;
  opacity: 0;
  transition: opacity 0.1s ease-in;
}

.notebook-gap:hover,
.notebook-gap:focus-within,
.notebook-gap-open {
  opacity: 1;
}

.gap-button {
  border: 1px solid var(--border);
  border-radius: var(--radius-sm);
  background: var(--surface);
  padding: 0 var(--space-sm);
  font-size: var(--text-xs);
  color: inherit;
  cursor: pointer;
}

.gap-button:hover {
  background: var(--surface-muted);
}
</style>
