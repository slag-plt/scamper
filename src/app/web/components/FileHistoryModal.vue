<script setup lang="ts">
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { EditorView } from '@codemirror/view'
import AppModal from './AppModal.vue'
import {
  editorThemeCompartment,
  editorThemeExtension,
  mkDiffEditorState,
} from '../codemirror/codemirror'
import { currentTheme } from '../../../theme'
import {
  formatSnapshotTime,
  type HistoryFile,
  type SnapshotRef,
} from '../../../history'

// Browses a file's saved history (issue #42). Purely presentational: the host
// loads the snapshots and performs the restore, so this component never
// touches the file system.
//
// It is handed the snapshot *times* and asks for one version's contents at a
// time, via `select-snapshot`. A server-backed history keeps each snapshot as
// its own row, so drawing this list costs no file contents at all; loading
// every version up front to render a column of timestamps would undo that.

const props = defineProps<{
  open: boolean
  /** Every file with a history, so a deleted one can still be reached. */
  files: HistoryFile[]
  /** Which of them is being shown. */
  filename: string
  /** The file's snapshots, newest first. Times only -- no contents. */
  snapshots: SnapshotRef[]
  /**
   * The selected snapshot's contents, or null while they are still being
   * fetched. The host loads them in response to `select-snapshot`.
   */
  selectedContents: string | null
  /**
   * What the editor holds right now, shown on top of the timeline and diffed
   * against. Null when the selected file isn't the one open in the editor --
   * a deleted file has no current version to compare with.
   */
  currentContents: string | null
}>()

const emit = defineEmits<{
  close: []
  select: [filename: string]
  /** Asks the host for this version's contents; null for the current row. */
  selectSnapshot: [snapshot: SnapshotRef | null]
  restore: [snapshot: SnapshotRef]
}>()

const isDeleted = computed(
  () => props.files.find((f) => f.filename === props.filename)?.deletedAt !== undefined,
)

// Which snapshot is selected, by index. -1 is the "current" row.
const selectedIndex = ref(-1)
const diffContainer = ref<HTMLDivElement | null>(null)
let diffView: EditorView | null = null

const selected = computed<SnapshotRef | null>(() =>
  selectedIndex.value < 0 ? null : (props.snapshots[selectedIndex.value] ?? null),
)

// Whatever is selected, the host has to be asked for its contents -- including
// the null that means "the current version", which clears a stale fetch.
watch(selected, (snapshot) => {
  if (props.open) emit('selectSnapshot', snapshot)
})

// Opening -- or switching to another file -- should always land on the newest
// version rather than wherever the last visit left off. Keyed on the snapshots
// too, since the host loads them a tick after the file name changes.
watch(
  [() => props.open, () => props.filename, () => props.snapshots],
  ([open]) => {
    if (open) selectedIndex.value = props.snapshots.length > 0 ? 0 : -1
  },
  { immediate: true },
)

/** @returns the label for a snapshot row. */
function labelFor(snapshot: SnapshotRef): string {
  return formatSnapshotTime(snapshot.time, new Date())
}

// Rebuilds the diff whenever the selection (or the dialog) changes. The view is
// recreated rather than reconfigured: a merge view's original document is fixed
// at construction, and these documents are a couple of KB.
watch(
  [selected, () => props.selectedContents, () => props.open, diffContainer],
  ([snapshot, contents, open, container]) => {
    diffView?.destroy()
    diffView = null
    // `contents` arrives a moment after the selection does, so this runs twice
    // per pick: once to tear the old diff down, once to build the new one.
    if (!open || container === null || snapshot === null) return
    if (contents === null) return
    // With no current version to compare against, the snapshot is diffed with
    // itself, which shows it plainly with no changes marked.
    const current = props.currentContents ?? contents
    diffView = new EditorView({
      state: mkDiffEditorState(current, contents),
      parent: container,
    })
  },
  { flush: 'post' },
)

// The diff pane is nearly all of this dialog, so it has to follow a theme
// toggle rather than sit in the old one until it is rebuilt (as
// CodeMirrorEditor.vue does for the editor).
watch(currentTheme, (theme) => {
  diffView?.dispatch({
    effects: editorThemeCompartment.reconfigure(editorThemeExtension(theme)),
  })
})

onBeforeUnmount(() => {
  diffView?.destroy()
  diffView = null
})

function restoreSelected() {
  if (selected.value !== null) emit('restore', selected.value)
}
</script>

<template>
  <AppModal :open="props.open" title="File history" @dismiss="emit('close')">
    <label class="picker">
      File:
      <select
        :value="props.filename"
        @change="emit('select', ($event.target as HTMLSelectElement).value)"
      >
        <option v-for="file in props.files" :key="file.filename" :value="file.filename">
          {{ file.filename }}{{ file.deletedAt ? ' (deleted)' : '' }}
        </option>
      </select>
    </label>
    <div class="history">
      <ul class="timeline" role="listbox" aria-label="Saved versions">
        <li v-if="props.currentContents !== null">
          <button
            type="button"
            role="option"
            class="entry"
            :class="{ selected: selectedIndex === -1 }"
            :aria-selected="selectedIndex === -1"
            @click="selectedIndex = -1"
          >
            Current version
          </button>
        </li>
        <li v-for="(snapshot, index) in props.snapshots" :key="snapshot.id">
          <button
            type="button"
            role="option"
            class="entry"
            :class="{ selected: selectedIndex === index }"
            :aria-selected="selectedIndex === index"
            @click="selectedIndex = index"
          >
            {{ labelFor(snapshot) }}
            <span v-if="index === 0" class="tag">newest</span>
          </button>
        </li>
      </ul>
      <div class="preview">
        <p v-if="props.snapshots.length === 0" class="empty">
          This file has no saved versions yet. Scamper records one every so
          often as you work.
        </p>
        <p v-else-if="selected === null" class="empty">
          Pick a saved version to see what has changed since then.
        </p>
        <template v-else>
          <p class="caption">
            {{
              props.currentContents === null
                ? `Saved at ${labelFor(selected)}:`
                : `Changes since ${labelFor(selected)}:`
            }}
          </p>
          <p v-if="props.selectedContents === null" class="empty">
            Loading this version...
          </p>
          <div v-show="props.selectedContents !== null" ref="diffContainer" class="diff"></div>
        </template>
      </div>
    </div>
    <template #footer>
      <span v-if="selected !== null" class="reassurance">
        {{
          isDeleted
            ? 'Recovering brings the file back with these contents.'
            : 'Restoring keeps your current version in the history.'
        }}
      </span>
      <button type="button" class="modal-button" @click="emit('close')">
        Close
      </button>
      <button
        type="button"
        class="modal-button modal-button--primary"
        :disabled="selected === null"
        @click="restoreSelected()"
      >
        {{ isDeleted ? 'Recover this version' : 'Restore this version' }}
      </button>
    </template>
  </AppModal>
</template>

<style scoped>
.picker {
  display: block;
  margin-bottom: 0.6em;
  font-size: 0.9em;
}

.picker select {
  margin-left: 0.4em;
  padding: 0.2em 0.3em;
  border: 1px solid var(--border);
  border-radius: 4px;
  background-color: var(--surface);
  color: var(--fg);
  font: inherit;
}

.history {
  display: flex;
  gap: 0.75rem;
  width: min(70rem, 80vw);
  height: min(28rem, 60vh);
}

.timeline {
  flex: 0 0 12rem;
  overflow-y: auto;
  margin: 0;
  padding: 0;
  list-style: none;
  border: 1px solid var(--border-muted);
  border-radius: 6px;
}

.entry {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  gap: 0.5em;
  width: 100%;
  padding: 0.4em 0.6em;
  border: none;
  background: none;
  color: inherit;
  font: inherit;
  text-align: left;
  cursor: pointer;
}

.entry:hover {
  background: var(--surface-hover);
}

.entry.selected {
  background: var(--accent);
  color: var(--accent-fg);
}

.tag {
  font-size: 0.75em;
  opacity: 0.7;
}

.preview {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
}

.caption {
  margin: 0 0 0.4em;
  font-size: 0.9em;
  opacity: 0.8;
}

.empty {
  margin: 0;
  opacity: 0.8;
}

.diff {
  flex: 1;
  min-height: 0;
  overflow: auto;
  border: 1px solid var(--border-muted);
  border-radius: 6px;
}

.reassurance {
  margin-right: auto;
  font-size: 0.85em;
  opacity: 0.8;
}

.modal-button {
  padding: 0.4rem 0.9rem;
  border: 1px solid var(--border);
  border-radius: 6px;
  background-color: var(--surface);
  color: var(--fg);
  font: inherit;
  cursor: pointer;
}

.modal-button:hover:not(:disabled) {
  background-color: var(--surface-hover);
}

.modal-button:disabled {
  opacity: 0.5;
  cursor: default;
}

.modal-button--primary {
  border-color: var(--accent);
  background-color: var(--accent);
  color: var(--accent-fg);
}
</style>
