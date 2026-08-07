<script setup lang="ts">
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { EditorView } from '@codemirror/view'
import AppModal from './AppModal.vue'
import { mkDiffEditorState } from '../codemirror/codemirror'
import { formatSnapshotTime, type Snapshot } from '../file-history'

// Browses a file's saved history (issue #42). Purely presentational: the host
// loads the snapshots and performs the restore, so this component never
// touches the file system.

const props = defineProps<{
  open: boolean
  filename: string
  /** The file's snapshots, newest first. */
  snapshots: Snapshot[]
  /** What the editor holds right now, which the timeline shows on top. */
  currentContents: string
}>()

const emit = defineEmits<{ close: []; restore: [snapshot: Snapshot] }>()

// Which snapshot is selected, by index. -1 is the "current" row.
const selectedIndex = ref(-1)
const diffContainer = ref<HTMLDivElement | null>(null)
let diffView: EditorView | null = null

const selected = computed<Snapshot | null>(() =>
  selectedIndex.value < 0 ? null : (props.snapshots[selectedIndex.value] ?? null),
)

// Reopening should always land on the newest version rather than wherever the
// last visit left off.
watch(
  () => props.open,
  (open) => {
    if (open) selectedIndex.value = props.snapshots.length > 0 ? 0 : -1
  },
  { immediate: true },
)

/** @returns the label for a snapshot row. */
function labelFor(snapshot: Snapshot): string {
  return formatSnapshotTime(snapshot.time, new Date())
}

// Rebuilds the diff whenever the selection (or the dialog) changes. The view is
// recreated rather than reconfigured: a merge view's original document is fixed
// at construction, and these documents are a couple of KB.
watch(
  [selected, () => props.open, diffContainer],
  ([snapshot, open, container]) => {
    diffView?.destroy()
    diffView = null
    if (!open || container === null || snapshot === null) return
    diffView = new EditorView({
      state: mkDiffEditorState(props.currentContents, snapshot.contents),
      parent: container,
    })
  },
  { flush: 'post' },
)

onBeforeUnmount(() => {
  diffView?.destroy()
  diffView = null
})

function restoreSelected() {
  if (selected.value !== null) emit('restore', selected.value)
}
</script>

<template>
  <AppModal
    :open="props.open"
    :title="`History of ${props.filename}`"
    @dismiss="emit('close')"
  >
    <div class="history">
      <ul class="timeline" role="listbox" aria-label="Saved versions">
        <li>
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
        <li v-for="(snapshot, index) in props.snapshots" :key="snapshot.time">
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
          <p class="caption">Changes since {{ labelFor(selected) }}:</p>
          <div ref="diffContainer" class="diff"></div>
        </template>
      </div>
    </div>
    <template #footer>
      <span v-if="selected !== null" class="reassurance">
        Restoring keeps your current version in the history.
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
        Restore this version
      </button>
    </template>
  </AppModal>
</template>

<style scoped>
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
