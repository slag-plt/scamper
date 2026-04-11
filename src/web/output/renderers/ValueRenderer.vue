<script setup lang="ts">
import { Value } from "../../../lpm"
import { computed } from "vue"
import { FallbackRenderer, renderStrategies } from "./render-strategies"

const props = defineProps<{ value: Value }>()

const resolvedComponent = computed(() => {
  const strategy = renderStrategies.find((s) => s.predicate(props.value))
  if (!strategy) return FallbackRenderer
  if (strategy.type === "vue") return strategy.renderer
  // TODO: this should do something with strategy.renderFn instead since its a dom one
  return FallbackRenderer
})
</script>

<template>
  <component :is="resolvedComponent" :value="value" />
</template>

<style scoped></style>
