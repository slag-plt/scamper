<script setup lang="ts">
import { computed, shallowRef } from "vue"
import ResultsToolbar from "./ResultsToolbar.vue"
import OutputPane from "./output/OutputPane.vue"
import type { OutputPaneType } from "./output/use-output-pane"

defineProps<{
  isTracing?: boolean
  isDirty?: boolean
  stepOnce?: () => void
  stepStmt?: () => Promise<void>
  stepAll?: () => Promise<void>
  astText?: () => void
  cancel?: () => void
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
  <ResultsToolbar
    :is-tracing="isTracing"
    :is-dirty="isDirty"
    :step-once="stepOnce"
    :step-stmt="stepStmt"
    :step-all="stepAll"
    :ast-text="astText"
    :cancel="cancel"
  />
  <div class="output-container">
    <OutputPane ref="outputPaneRef" />
  </div>
</template>

<style scoped>
.output-container {
  flex: 1;
  min-height: 0;
  background: #fff;
  color: #333;
}
</style>
