<script setup lang="ts">
import { SchedulerId } from "../../../../lpm/scheduler"
import { useScamperSession } from "../../../composables/use-scamper-session"
import { useReportedPageGraph } from "../query-utils"

const props = withDefaults(
  defineProps<{ queryId: SchedulerId; overflowing?: boolean }>(),
  { overflowing: true },
)

const {
  getQueryOrThrow,
  invalidateQuery,
  openRootPageModal,
  toggleQueryExpanded,
} = useScamperSession()

const pageGraph = useReportedPageGraph(() => getQueryOrThrow(props.queryId))
</script>

<template>
  <div id="query-controls">
    <button class="query-button" @click="invalidateQuery(props.queryId)">
      X
    </button>
    <button
      v-if="pageGraph"
      class="query-button"
      @click.stop="openRootPageModal(props.queryId, pageGraph.rootPage)"
    >
      {{ ">" }}
    </button>
    <button
      v-else-if="props.overflowing"
      class="query-button"
      @click.stop="toggleQueryExpanded(props.queryId)"
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
