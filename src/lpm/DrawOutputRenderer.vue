<script setup lang="ts">
import { TraceOutput } from "./trace"
//import CodeElement from "./renderers/vue/components/CodeElement.vue"
import ValueRenderer from "./renderers/vue/ValueRenderer.vue"

defineProps<{ value: TraceOutput }>()
</script>

<template>
  <div class="scamper-draw">
    <ValueRenderer :value="value.output" />
  </div>
</template>

<style scoped></style>
