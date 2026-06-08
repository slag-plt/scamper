<script setup lang="ts">
import { DrawStart } from "./trace"
import ValueRenderer from "./renderers/vue/ValueRenderer.vue"

defineProps<{ value: DrawStart }>()
</script>

<template>
  <div class="scamper-draw-start">
    {{ `${value.preamble} ` }}
    <ValueRenderer v-if="value.output != null" :value="value.output" />
  </div>
</template>

<style scoped></style>
