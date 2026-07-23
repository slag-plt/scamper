<script setup lang="ts">
import { QueryMap } from '../../../../scamper'
import QueryModal from './modal/QueryModal.vue'
import { ModalHeight } from './query-utils'

defineProps<{ queries: NonNullable<ReturnType<QueryMap['get']>> }>()

const height = `${ModalHeight.toString()}lh`
</script>

<template>
  <div class="query-container">
    <QueryModal v-for="q in queries" :key="q.id" :query="q" />
  </div>
</template>

<style scoped>
.query-container {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  width: 100%;
  height: v-bind("height");
}
</style>
