<script setup lang="ts">
import { List, Value } from "../../../index"
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
  
  <template v-for="(val, index) in flatList" :key="index">
    <!-- <CodeElement>{{ " " }}</CodeElement> -->
    <CodeElement>[ </CodeElement>
    <ValueRenderer :value="val" />
    <CodeElement> ][  ―]―▶</CodeElement>
  </template>
  <CodeElement>NULL</CodeElement>
  
</template>

<style scoped></style>
