<script setup lang="ts">
import { TraceOutput } from '../index'
import ValueRenderer from '../../renderers/vue/ValueRenderer.vue'

// One reduction step of a trace. Deliberately no "-->" marker: the web trace is
// paginated, so a step is already presented on its own and the arrow says
// nothing the view does not. The console trace keeps it (src/lpm/trace/index.ts)
// where the steps are consecutive lines with nothing else to separate them.
defineProps<{ value: TraceOutput }>()
</script>

<template>
  <div class="scamper-trace">
    <ValueRenderer :value="value.output" />
  </div>
</template>

<style scoped></style>
