<script setup lang="ts">
import { onMounted, ref, watch } from "vue"
import { Drawing, render, clearDrawing, canvasAriaLabel } from "./drawing"

const props = defineProps<{ value: Drawing }>()
const canvas = ref<HTMLCanvasElement | null>(null)

function renderDrawing() {
  if (canvas.value) {
    canvas.value.width = Math.ceil(props.value.width)
    canvas.value.height = Math.ceil(props.value.height)
    clearDrawing(canvas.value)
    render(0, 0, props.value, canvas.value)
  }
}

onMounted(renderDrawing)
watch(() => props.value, renderDrawing, { deep: true })
</script>

<template>
  <canvas ref="canvas" :aria-label="canvasAriaLabel"></canvas>
</template>
