<script setup lang="ts">
import { TraceOutput } from "./lpm/trace"
import CodeElement from "./web/output/renderers/components/CodeElement.vue"
import ValueRenderer from "./web/output/renderers/ValueRenderer.vue"

defineProps<{ value: TraceOutput }>()
</script>

<template class="scamper-trace">
  <CodeElement>{{ "--> " }}</CodeElement>
  <ValueRenderer :value="value.output" />
</template>

<style scoped></style>
