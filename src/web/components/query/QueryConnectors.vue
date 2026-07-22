<script setup lang="ts">
import { ConnectorHeight } from './query-utils'
import { QueryMap } from '../../../scamper'
import { Range } from '../../../lpm'
import { useEditor } from '../../composables/editor-context'
import { ref, shallowRef, watchPostEffect } from 'vue'

const props = defineProps<{
  queries: NonNullable<ReturnType<QueryMap['get']>>
  container: HTMLElement | null
}>()
const svgRef = ref<HTMLElement | null>(null)
const lines = shallowRef<{ id: string; x1: number; x2: number }[]>([])

const editor = useEditor()

const connectorHeight = `${ConnectorHeight.toString()}lh`

function midpointX(range: Range, svgLeft: number): number {
  const ed = editor()
  const from = ed.coordsAtIdx(range.begin.idx)
  const to = ed.coordsAtIdx(range.end.idx + 1)
  if (!from || !to) return 0
  return (from.left + to.left) / 2 - svgLeft
}

function modalBottomCenterX(modal: HTMLElement, svgLeft: number): number {
  const rect = modal.getBoundingClientRect()
  return (rect.left + rect.right) / 2 - svgLeft
}

watchPostEffect(() => {
  const svg = svgRef.value
  const container = props.container
  if (!svg || !container) return
  const origin = svg.getBoundingClientRect().left
  lines.value = props.queries.map((q) => {
    const modal = container.querySelector<HTMLElement>(
      `[data-query-id="${q.id}"]`,
    )
    return {
      id: q.id,
      x1: midpointX(q.queriedRange, origin),
      x2: modal ? modalBottomCenterX(modal, origin) : svg.clientWidth / 2,
    }
  })
})
</script>

<template>
  <svg ref="svgRef" class="connectors">
    <line
      v-for="line in lines"
      :key="line.id"
      :x1="line.x1"
      y1="100%"
      :x2="line.x2"
      y2="0%"
      stroke="black"
      stroke-width="2"
    ></line>
  </svg>
</template>

<style scoped>
.connectors {
  width: 100%;
  height: v-bind("connectorHeight");
}
</style>
