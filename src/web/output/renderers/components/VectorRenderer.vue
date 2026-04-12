<script setup lang="ts">
import { Value } from "../../../../lpm"
import CodeElement from "./CodeElement.vue"
import ValueRenderer from "../ValueRenderer.vue"

defineProps<{ value: Value[] }>()
</script>

<template>
  <CodeElement>(vector</CodeElement>
  <template v-for="[index, val] in value.entries()" :key="index">
    <CodeElement>{{ " " }}</CodeElement>
    <ValueRenderer :value="val" />
  </template>
  <CodeElement>)</CodeElement>
</template>

<style scoped></style>
