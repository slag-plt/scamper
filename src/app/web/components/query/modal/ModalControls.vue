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
    <button class="query-button" @click="invalidateQuery(queryId)">X</button>
    <button
      v-if="overflowing"
      class="query-button"
      @click.stop="toggleQueryExpanded(queryId)"
    >
      {{ "…" }}
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

.query-button {
  width: fit-content;
  font-size: 0.5lh;
}
</style>
