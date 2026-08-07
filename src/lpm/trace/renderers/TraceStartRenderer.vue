<script setup lang="ts">
import { TraceStart } from '../index'
import ValueRenderer from '../../renderers/vue/ValueRenderer.vue'

defineProps<{ value: TraceStart }>()
</script>

<template>
  <div class="scamper-trace-start">
    <template v-if="value.preamble !== ''">{{ `${value.preamble} ` }}</template>
    <ValueRenderer v-if="value.output != null" :value="value.output" />
  </div>
</template>

<style scoped></style>
