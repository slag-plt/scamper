<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from 'vue'
import Chart from 'chart.js/auto'
import { Plot } from '../viz'
import { onThemeChange, readColorToken } from '../../../theme'

const props = defineProps<{ value: Plot }>()
const canvas = ref<HTMLCanvasElement | null>(null)
let chart: Chart | null = null

// Theme axis/legend/title text and gridlines via Chart.js globals (user-supplied
// dataset colors are untouched). Applied before each render so charts pick up the
// current theme; re-render on theme change repaints existing charts.
function applyThemeDefaults() {
  Chart.defaults.color = readColorToken('--chart-fg')
  Chart.defaults.borderColor = readColorToken('--chart-grid')
}

function renderChart() {
  if (canvas.value) {
    applyThemeDefaults()
    if (chart) {
      chart.destroy()
    }
    chart = new Chart(canvas.value, props.value.opts as any)
  }
}

onMounted(renderChart)
watch(() => props.value, renderChart, { deep: true })
const unsubscribe = onThemeChange(renderChart)
onUnmounted(() => {
  unsubscribe()
  chart?.destroy()
  chart = null
})
</script>

<template>
  <div class="plot-container">
    <canvas ref="canvas" aria-label="Plot" role="img">Plot</canvas>
  </div>
</template>

<style scoped>
.plot-container {
  width: 800px;
  max-width: 100%;
}
</style>
