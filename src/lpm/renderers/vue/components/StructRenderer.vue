<script setup lang="ts">
import { getFieldsOfStruct, Struct, structKind } from "../../../index"
import { computed } from "vue"
import CodeElement from "./CodeElement.vue"
import ValueRenderer from "../ValueRenderer.vue"

const props = defineProps<{ value: Struct }>()
const fields = computed(() => {
  return getFieldsOfStruct(props.value)
})
</script>

<template>
  <CodeElement>({{ `${value[structKind]}` }}</CodeElement>
  <template v-for="(field, index) in fields" :key="index">
    <CodeElement>{{ " " }}</CodeElement>
    <ValueRenderer :value="value[field]" />
  </template>
  <CodeElement>)</CodeElement>
</template>

<style scoped></style>
