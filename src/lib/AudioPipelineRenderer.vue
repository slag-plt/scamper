<script setup lang="ts">
import { ref, onUnmounted } from "vue"
import { AudioPipeline, isMediaElementSource } from "./audio"

const props = defineProps<{ value: AudioPipeline }>()

const started = ref(false)

function play() {
  const onOffNode = props.value.onOffNode
  const pipeline = props.value.pipeline
  onOffNode.gain.value = 1

  if (
    "start" in pipeline &&
    typeof (pipeline as any).start === "function" &&
    !started.value
  ) {
    ;(pipeline as AudioScheduledSourceNode).start()
    started.value = true
  } else if (isMediaElementSource(pipeline)) {
    pipeline.mediaElement.play().catch(console.error)
    started.value = true
  }
}

function stop() {
  const onOffNode = props.value.onOffNode
  const pipeline = props.value.pipeline
  onOffNode.gain.value = 0

  if (isMediaElementSource(pipeline)) {
    pipeline.mediaElement.load()
    started.value = false
  }
}

onUnmounted(stop)
</script>

<template>
  <span>
    <button @click="play">▶</button>
    <button @click="stop">■</button>
  </span>
</template>
