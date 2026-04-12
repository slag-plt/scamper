<script setup lang="ts">
import { Pair } from "../../../../lpm"
import CodeElement from "./CodeElement.vue"
import ValueRenderer from "../ValueRenderer.vue"

defineProps<{ value: Pair }>()
</script>

<template>
  <CodeElement>(pair</CodeElement>
  <CodeElement>{{ " " }}</CodeElement>
  <ValueRenderer :value="value.fst" />
  <CodeElement>{{ " " }}</CodeElement>
  <ValueRenderer :value="value.snd" />
  <CodeElement>)</CodeElement>
</template>

<style scoped></style>
