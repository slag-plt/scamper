<script setup lang="ts">
import { onMounted, ref, watch } from "vue"
import Chart from "chart.js/auto"
import { Plot } from "./viz"

const props = defineProps<{ value: Plot }>()
const canvas = ref<HTMLCanvasElement | null>(null)
let chart: Chart | null = null

function renderChart() {
  if (canvas.value) {
    if (chart) {
      chart.destroy()
    }
    chart = new Chart(canvas.value, props.value.opts as any)
  }
}

onMounted(renderChart)
watch(() => props.value, renderChart, { deep: true })
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
