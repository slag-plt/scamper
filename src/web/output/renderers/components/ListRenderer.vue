<script setup lang="ts">
import { List, Value } from "../../../../lpm"
import CodeElement from "./CodeElement.vue"
import ValueRenderer from "../ValueRenderer.vue"
import { computed } from "vue"

const props = defineProps<{ value: List }>()

const flatList = computed(() => {
  // continuously extract head out of tail
  const result: Value[] = []
  let curr = props.value
  while (curr !== null) {
    result.push(curr.head)
    curr = curr.tail
  }
  return result
})
</script>

<template>
  <CodeElement>(list</CodeElement>
  <template v-for="(val, index) in flatList" :key="index">
    <CodeElement>{{ " " }}</CodeElement>
    <ValueRenderer :value="val" />
  </template>
  <CodeElement>)</CodeElement>
</template>

<style scoped></style>
