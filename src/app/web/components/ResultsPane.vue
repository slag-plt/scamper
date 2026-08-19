<script setup lang="ts">
import { computed, shallowRef } from 'vue'
import ResultsToolbar from './ResultsToolbar.vue'
import OutputPane from './OutputPane.vue'
import type { OutputPaneType } from '../composables/use-output-pane'

defineProps<{
  isDirty: boolean
}>()

const outputPaneRef = shallowRef<OutputPaneType | null>(null)

const display = computed(() => outputPaneRef.value?.display)

defineExpose({
  reset: () => outputPaneRef.value?.reset(),
  scrollToBottom: () => outputPaneRef.value?.scrollToBottom(),
  display,
})
</script>

<template>
  <ResultsToolbar :is-dirty="isDirty" />
  <div class="output-container">
    <OutputPane ref="outputPaneRef" />
  </div>
</template>

<style scoped>
.output-container {
  flex: 1;
  min-height: 0;
  background: var(--surface);
  color: var(--fg);
}
</style>
