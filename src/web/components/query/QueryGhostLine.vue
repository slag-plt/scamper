<script setup lang="ts">
import { QueryMap } from "../../../scamper"
import { getIdForGhostLine } from "../../codemirror/extensions/query"
import { type ComponentPublicInstance, computed, ref } from "vue"
import QueryContainer from "./QueryContainer.vue"
import { ModalCols } from "./query-utils"
import QueryConnectors from "./QueryConnectors.vue"

const { line, queries } = defineProps<{
  line: number
  queries: NonNullable<ReturnType<QueryMap["get"]>>
}>()

const width = computed(() => {
  // TODO: get this from query modals
  const gaps = queries.length - 1
  return ModalCols * queries.length + gaps
})

const leftOffset = computed(() => {
  const [leftCol, rightCol] = queries.reduce(
    ([left, right], q) => {
      return [
        q.queriedRange.begin.col < left ? q.queriedRange.begin.col : left,
        q.queriedRange.end.col + 1 > right ? q.queriedRange.end.col + 1 : right,
      ]
    },
    [Infinity, -1],
  )

  if (!Number.isFinite(leftCol) || rightCol === -1) {
    console.error(`somehow got bad left offset for ${line.toString()}`)
    return 0
  }

  const colWidth = rightCol - leftCol
  const center = leftCol + colWidth / 2
  const offset = center - width.value / 2

  return offset < 0 ? 0 : offset
})

const containerRef = ref<ComponentPublicInstance | null>(null)
const containerEl = computed(
  () => (containerRef.value?.$el ?? null) as HTMLElement | null,
)
</script>

<template>
  <Teleport :to="`#${getIdForGhostLine(line)}`" defer>
    <div id="ghost-line">
      <QueryContainer ref="containerRef" :queries="queries" />
      <QueryConnectors :queries="queries" :container="containerEl" />
    </div>
  </Teleport>
</template>

<style scoped>
#ghost-line {
  width: v-bind("`${width}ch`");
  height: 100%;
  left: v-bind("`${leftOffset}ch`");
  position: relative;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
}
</style>
