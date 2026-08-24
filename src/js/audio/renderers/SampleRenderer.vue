<script setup lang="ts">
import { ref } from 'vue'
import { SampleNode, audio_getCtx, sampleSourceNode } from '../index'
import { drawOscilloscope } from './html'

const props = defineProps<{ value: SampleNode }>()

const visualizer = ref<HTMLCanvasElement | null>(null)
let source: AudioBufferSourceNode | undefined

function play() {
  const ctx = audio_getCtx()
  const analyser = ctx.createAnalyser()
  analyser.fftSize = 2048
  const bufferLength = analyser.frequencyBinCount
  const dataArray = new Uint8Array(bufferLength)
  analyser.getByteTimeDomainData(dataArray)

  // A fresh source per press: one can only be started once.
  source = sampleSourceNode(ctx, props.value)
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
