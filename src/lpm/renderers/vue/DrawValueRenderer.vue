<script setup lang="ts">
import { Value } from "../../index"
import { computed } from "vue"
import VueRenderer from "../vue"

const props = defineProps<{ value: Value }>()

const resolvedComponent = computed(() => {
  return VueRenderer.render(props.value)
})
</script>

<template>
  <component :is="resolvedComponent" :value="value" />
</template>

<style scoped></style>
