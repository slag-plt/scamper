<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from 'vue'
import { Drawing, drawing_normalize, drawing_render, drawing_clearDrawing, drawing_canvasAriaLabel } from '../drawing'
import { onThemeChange, readColorToken } from '../../../theme'

const props = defineProps<{ value: Drawing }>()
const canvas = ref<HTMLCanvasElement | null>(null)

function renderDrawing() {
  // The IDE's output pane bypasses drawing_renderer, so it normalises for
  // itself: a nested rotation must be sized and painted collapsed here too.
  const d = drawing_normalize(props.value)
  if (canvas.value) {
    canvas.value.width = Math.ceil(d.width)
    canvas.value.height = Math.ceil(d.height)
    // Themed background for display so the drawing blends into the page instead
    // of being a white box on a dark background.
    drawing_clearDrawing(canvas.value, readColorToken('--canvas-surface'))
    drawing_render(0, 0, d, canvas.value)
  }
}

onMounted(renderDrawing)
watch(() => props.value, renderDrawing, { deep: true })
// Repaint on theme toggle (see PlotRenderer.vue for the same pattern).
const unsubscribe = onThemeChange(renderDrawing)
onUnmounted(unsubscribe)
</script>

<template>
  <canvas ref="canvas" :aria-label="drawing_canvasAriaLabel"></canvas>
</template>
