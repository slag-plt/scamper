<script setup lang="ts">
import { TraceOutput } from "../index"
import CodeElement from "../../renderers/vue/components/CodeElement.vue"
import ValueRenderer from "../../renderers/vue/ValueRenderer.vue"

defineProps<{ value: TraceOutput }>()
</script>

<template>
  <div class="scamper-trace">
    <CodeElement>{{ "--> " }}</CodeElement>
    <ValueRenderer :value="value.output" />
  </div>
</template>

<style scoped></style>
