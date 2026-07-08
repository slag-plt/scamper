<script setup lang="ts">
import { QueryMap } from "../../../scamper"
import { getIdForGhostLine } from "../../codemirror/extensions/query"
import { computed } from "vue"

const { line, queries } = defineProps<{
  line: number
  queries: NonNullable<ReturnType<QueryMap["get"]>>
}>()

const leftOffset = computed(() => {
  const leftIdx = queries.reduce((left, q) => {
    return q.queriedRange.begin.col < left ? q.queriedRange.begin.col : left
  }, Infinity)

  if (!Number.isFinite(leftIdx)) {
    console.error(`somehow got bad left offset for ${line.toString()}`)
    return 0
  }
  return leftIdx
})

const width = computed(() => {
  // TODO: get this from query modals
  const standardModalWidth = 8
  const gaps = queries.length - 1
  return standardModalWidth * queries.length + gaps
})
</script>

<template>
  <Teleport :to="`#${getIdForGhostLine(line)}`" defer>
    <div id="query-container"></div>
  </Teleport>
</template>

<style scoped>
#query-container {
  display: flex;
  flex-direction: row;
  width: v-bind("`${width}ch`");
  height: 100%;
  left: v-bind("`${leftOffset}ch`");
  position: relative;
  background-color: red;
}
</style>
