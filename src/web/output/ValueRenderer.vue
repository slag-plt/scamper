<script setup lang="ts">
import { Value } from "../../lpm"
import { computed } from "vue"
import { renderStrategies } from "./render-strategies"
import FallbackRenderer from "./renderers/FallbackRenderer.vue"

const props = defineProps<{ value: Value }>()

const resolvedComponent = computed(() => {
  const strategy = renderStrategies.find((s) => s.predicate(props.value))
  if (!strategy) return FallbackRenderer
  if (strategy.type === "vue") return strategy.renderer
  // TODO: this should do something with strategy.renderFn instead
  return FallbackRenderer
})
</script>

<template>
  <component :is="resolvedComponent" :value="value" />
</template>

<style scoped></style>
