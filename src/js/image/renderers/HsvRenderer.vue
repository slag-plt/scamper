<script setup lang="ts">
import { computed } from 'vue'
import {
  Hsv,
  color_hsvToRgb,
  color_hsvToString,
  color_rgbToString,
  color_rgbPseudoComplement,
} from '../color'

const props = defineProps<{ value: Hsv }>()

const rgbValue = computed(() => color_hsvToRgb(props.value))
const backgroundColor = computed(() => color_rgbToString(rgbValue.value))
const textColor = computed(() =>
  color_rgbToString(color_rgbPseudoComplement(rgbValue.value)),
)
const displayText = computed(() => color_hsvToString(props.value))
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
    {{ displayText }}
  </div>
</template>
