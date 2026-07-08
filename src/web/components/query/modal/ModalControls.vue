<script setup lang="ts">
import { useScamperSession } from "../../../composables/use-scamper-session"
import { SchedulerId } from "../../../../scheduler"

defineProps<{ id: SchedulerId }>()

const { invalidateQuery } = useScamperSession()
</script>

<template>
  <div id="query-controls">
    <button class="query-close-button" @click="invalidateQuery(id)">X</button>
    <!--  TODO: add expand button  -->
  </div>
</template>

<style scoped>
#query-controls {
  min-width: fit-content;
  flex-shrink: 0;
  display: flex;
  flex-direction: column;
}

.query-close-button {
  font-size: 0.5lh;
}
</style>
