<script setup lang="ts">
import {
  showSourceWithOutput,
  toggleShowSourceWithOutput,
} from '../output-prefs'

/**
 * The output window's own strip of controls.
 *
 * Only what is about how the output is *shown* lives here. Stepping used to,
 * back when a trace ran in place and had to be driven from beside it; it now
 * has its own window with its own controls, and the buttons here were left
 * pointing at a mode nothing starts any more.
 */
defineProps<{
  isDirty: boolean
}>()
</script>

<template>
  <div class="results-toolbar">
    <div>
      <button
        type="button"
        class="icon-button fa-solid fa-code"
        :class="{ active: showSourceWithOutput }"
        :title="
          showSourceWithOutput
            ? 'Hide the source of each result'
            : 'Show the source of each result'
        "
        aria-label="Show source with output"
        role="switch"
        :aria-checked="showSourceWithOutput"
        @click="toggleShowSourceWithOutput()"
      ></button>
    </div>
    <div v-if="isDirty" class="results-status">
      <em>(Warning: results out of sync with updated code)</em>
    </div>
  </div>
</template>

<style scoped>
/* A strip, like the trace window's controls: it has to hold its height in the
   window's flex column and read as chrome rather than as the first result. */
.results-toolbar {
  flex-shrink: 0;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 0.5em;
  padding: 0.25em 0.5em;
  background: var(--surface-muted);
  border-bottom: 1px solid var(--border);
}

.results-toolbar > div {
  display: flex;
  align-items: center;
  gap: 0.25em;
}

.results-status {
  min-width: 0;
  font-size: 0.85em;
  color: var(--fg);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

/* A toggle rather than a command, so it has to look pressed while it is on. */
.active {
  background: var(--accent);
  color: var(--accent-fg);
  border-radius: 3px;
}
</style>
