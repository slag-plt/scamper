<script setup lang="ts">
import { TraceOutput } from "./trace"
import CodeElement from "./renderers/vue/components/CodeElement.vue"
import ValueRenderer from "./renderers/vue/ValueRenderer.vue"

defineProps<{ value: TraceOutput }>()
</script>

<template class="scamper-trace">
  <CodeElement>{{ "--> " }}</CodeElement>
  <ValueRenderer :value="value.output" />
</template>

<style scoped></style>
