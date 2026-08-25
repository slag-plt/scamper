<script setup lang="ts">
import { TraceOutput } from '../index'
import ValueRenderer from '../../renderers/vue/ValueRenderer.vue'

// One reduction step of a trace. Deliberately no "-->" marker: every web
// surface gives a step a container of its own -- a row in the output pane, a
// page in the trace window -- so the arrow says nothing the view does not. The
// console trace keeps it (src/lpm/trace/index.ts): there a step is a bare line
// among other bare lines, with nothing else to mark where one begins.
defineProps<{ value: TraceOutput }>()
</script>

<template>
  <div class="scamper-trace">
    <ValueRenderer :value="value.output" />
  </div>
</template>

<style scoped></style>
