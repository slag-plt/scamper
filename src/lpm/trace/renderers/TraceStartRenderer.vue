<script setup lang="ts">
import { TraceStart } from '../index'
import ValueRenderer from '../../renderers/vue/ValueRenderer.vue'

defineProps<{ value: TraceStart }>()
</script>

<template>
  <div class="scamper-trace-start">
    {{ `${value.preamble} ` }}
    <ValueRenderer v-if="value.output != null" :value="value.output" />
  </div>
</template>

<style scoped></style>
