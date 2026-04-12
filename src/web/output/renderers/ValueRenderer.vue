<script setup lang="ts">
import { Value } from "../../../lpm"
import { computed } from "vue"
import VueRenderer from "./VueRenderer"

const props = defineProps<{ value: Value }>()

const resolvedComponent = computed(() => {
  return VueRenderer.render(props.value)
})
</script>

<template>
  <component :is="resolvedComponent" :value="value" />
</template>

<style scoped></style>
