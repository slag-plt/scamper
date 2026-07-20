<script setup lang="ts">
import { onMounted, ref, watch } from "vue"
import { Drawing, image_render, image_clearDrawing, image_canvasAriaLabel } from "../drawing"

const props = defineProps<{ value: Drawing }>()
const canvas = ref<HTMLCanvasElement | null>(null)

function renderDrawing() {
  if (canvas.value) {
    canvas.value.width = Math.ceil(props.value.width)
    canvas.value.height = Math.ceil(props.value.height)
    image_clearDrawing(canvas.value)
    image_render(0, 0, props.value, canvas.value)
  }
}

onMounted(renderDrawing)
watch(() => props.value, renderDrawing, { deep: true })
</script>

<template>
  <canvas ref="canvas" :aria-label="image_canvasAriaLabel"></canvas>
</template>
