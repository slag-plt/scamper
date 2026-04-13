<script setup lang="ts">
import { TraceStart } from "./trace"
import ValueRenderer from "./renderers/vue/ValueRenderer.vue"

defineProps<{ value: TraceStart }>()
</script>

<template class="scamper-trace-start">
  {{ `${value.preamble} ` }}
  <ValueRenderer v-if="value.output != null" :value="value.output" />
</template>

<style scoped></style>
