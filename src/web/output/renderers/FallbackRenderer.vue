<script setup lang="ts">
import CodeElement from "./CodeElement.vue"
import { Value } from "../../../lpm"
defineProps<{ value: Value }>()
</script>

<template>
  <CodeElement>{{ `[Blob: ${JSON.stringify(value)}]` }}</CodeElement>
</template>

<style scoped></style>
