<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from 'vue'
import { Drawing, image_render, image_clearDrawing, image_canvasAriaLabel } from '../drawing'
import { onThemeChange, readColorToken } from '../../../theme'

const props = defineProps<{ value: Drawing }>()
const canvas = ref<HTMLCanvasElement | null>(null)

function renderDrawing() {
  if (canvas.value) {
    canvas.value.width = Math.ceil(props.value.width)
    canvas.value.height = Math.ceil(props.value.height)
    // Themed background for display so the drawing blends into the page instead
    // of being a white box on a dark background.
    image_clearDrawing(canvas.value, readColorToken('--canvas-surface'))
    image_render(0, 0, props.value, canvas.value)
  }
}

onMounted(renderDrawing)
watch(() => props.value, renderDrawing, { deep: true })
// Repaint on theme toggle (see PlotRenderer.vue for the same pattern).
const unsubscribe = onThemeChange(renderDrawing)
onUnmounted(unsubscribe)
</script>

<template>
  <canvas ref="canvas" :aria-label="image_canvasAriaLabel"></canvas>
</template>
