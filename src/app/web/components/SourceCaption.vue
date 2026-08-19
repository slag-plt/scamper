<script setup lang="ts">
import { computed } from 'vue'
import { highlightScamper } from '../source-highlight'
import { showSourceWithOutput } from '../output-prefs'

/**
 * The statement that produced the output below it, shown above that output as a
 * highlighted, non-editable box.
 *
 * Always rendered and hidden with `display: none` rather than dropped from the
 * DOM, so turning the option on captions output already on screen instead of
 * only the next run's.
 */
const props = withDefaults(
  defineProps<{
    source: string
    /**
     * Shows the source regardless of the option. The trace window uses this:
     * the statement being stepped is that window's subject, not a caption on
     * someone else's output, so hiding it would leave the window unlabelled.
     */
    forceVisible?: boolean
  }>(),
  { forceVisible: false },
)

const tokens = computed(() => highlightScamper(props.source))
const shown = computed(() => props.forceVisible || showSourceWithOutput.value)
</script>

<template>
  <div v-show="shown" class="source-caption">
    <code
      ><span
        v-for="(token, i) in tokens"
        :key="i"
        :class="token.cls ?? undefined"
        >{{ token.text }}</span
      ></code
    >
  </div>
</template>

<style scoped>
/* Tinted and ruled down its left edge, so at a glance it reads as the code
   that caused the output rather than as more output. */
.source-caption {
  margin: 0.4em 0 0.2em;
  padding: 0.3em 0.6em;
  background: var(--surface-muted);
  border: 1px solid var(--border);
  border-left: 3px solid var(--brand);
  border-radius: 4px;
  overflow-x: auto;
}

code {
  font-family:
    Menlo, Consolas, Monaco, "Liberation Mono", "Lucida Console", monospace;
  font-size: 0.85em;
  white-space: pre;
  /* Nothing here is editable; keep the caret and the edit affordances away. */
  user-select: text;
  cursor: default;
}
</style>
