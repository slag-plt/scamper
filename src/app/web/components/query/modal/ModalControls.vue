<script setup lang="ts">
import { SchedulerId } from '../../../../../lpm/scheduler'
import { useScamperSession } from '../../../composables/use-scamper-session'

withDefaults(defineProps<{ queryId: SchedulerId; overflowing?: boolean }>(), {
  overflowing: true,
})

const { invalidateQuery, toggleQueryExpanded } = useScamperSession()
</script>

<template>
  <div id="query-controls">
    <button
      type="button"
      class="query-button"
      title="Dismiss this value"
      aria-label="Dismiss this value"
      @click="invalidateQuery(queryId)"
    >
      &times;
    </button>
    <button
      v-if="overflowing"
      type="button"
      class="query-button"
      title="Show the whole value"
      aria-label="Show the whole value"
      @click.stop="toggleQueryExpanded(queryId)"
    >
      &hellip;
    </button>
  </div>
</template>

<style scoped>
#query-controls {
  min-width: fit-content;
  flex-shrink: 0;
  display: flex;
  flex-direction: column;
}

/*
 * These were unlabelled `X` and `…` text at 0.5lh -- around 7px, well under
 * WCAG 2.2 SC 2.5.8's 24x24 minimum, and silent to a screen reader.
 *
 * 1.5rem is that minimum. The glyph stays small; it is the hit area that grows,
 * which does make the query card a few pixels taller than it was. That is the
 * trade, and it is the right way round.
 */
.query-button {
  display: grid;
  place-items: center;
  min-width: 1.5rem;
  min-height: 1.5rem;
  padding: 0;
  font: inherit;
  font-size: var(--text-xs);
  line-height: 1;
  color: inherit;
  background: none;
  border: none;
  border-radius: var(--radius-sm);
  cursor: pointer;
  opacity: 0.6;
}

.query-button:hover {
  opacity: 1;
  background: var(--surface-hover);
}
</style>
