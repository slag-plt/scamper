<script setup lang="ts">
import { ref } from "vue"
import { SampleNode, getCtx, drawOscilloscope } from "./audio"

const props = defineProps<{ value: SampleNode }>()

const visualizer = ref<HTMLCanvasElement | null>(null)
let source: AudioBufferSourceNode | undefined

function play() {
  const ctx = getCtx()
  const analyser = ctx.createAnalyser()
  analyser.fftSize = 2048
  const bufferLength = analyser.frequencyBinCount
  const dataArray = new Uint8Array(bufferLength)
  analyser.getByteTimeDomainData(dataArray)

  const data = props.value.data
  const buffer = ctx.createBuffer(2, data.length, ctx.sampleRate)
  buffer.copyToChannel(data, 0)
  buffer.copyToChannel(data, 1)

  source = ctx.createBufferSource()
  source.buffer = buffer
  source.connect(ctx.destination)
  source.connect(analyser)
  source.start()

  if (visualizer.value) {
    drawOscilloscope(dataArray, visualizer.value, analyser)
  }
}

function stop() {
  if (source !== undefined) {
    source.stop()
    source = undefined
  }
}
</script>

<template>
  <span>
    <button @click="play">▶</button>
    <button @click="stop">■</button>
    <canvas ref="visualizer"></canvas>
  </span>
</template>
