<!-- A swatch of `color` with `label` written across it, in whichever of black
     or white reads better over the swatch (see `swatch.ts`). -->
<script setup lang="ts">
import { computed } from 'vue'
import { Rgb, color_rgbToString } from '../color'
import { swatchFill, swatchInk } from './swatch'

const props = defineProps<{ color: Rgb, label: string }>()

const backgroundColor = computed(() => color_rgbToString(swatchFill(props.color)))
const textColor = computed(() => swatchInk(props.color))
</script>

<template>
  <div
    :style="{
      color: textColor,
      backgroundColor: backgroundColor,
      width: 'fit-content',
      border: '1px solid var(--border)',
      padding: '0.25em',
    }"
  >
    {{ label }}
  </div>
</template>
