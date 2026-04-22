<script setup lang="ts">
import { ref, onUnmounted } from "vue"
import { Composition, playComposition } from "./music"
import { waf } from "./webaudiofont/webaudiofont"

const props = defineProps<{ value: Composition }>()

const timer = ref<number | undefined>(undefined)

async function play() {
  const player = waf()
  if (!player) return

  if (player.audioContext.state === "suspended") {
    await player.audioContext.resume().catch(console.error)
  }
  timer.value = playComposition(props.value)
}

function stop() {
  const player = waf()
  if (timer.value !== undefined) {
    window.clearInterval(timer.value)
    timer.value = undefined
  }
  if (player) {
    player.player.cancelQueue(player.audioContext)
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
