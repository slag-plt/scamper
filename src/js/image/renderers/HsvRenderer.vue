<script setup lang="ts">
import { computed } from "vue"
import {
  Hsv,
  image_hsvToRgb,
  image_hsvToString,
  image_rgbToString,
  image_rgbPseudoComplement,
} from "../color"

const props = defineProps<{ value: Hsv }>()

const rgbValue = computed(() => image_hsvToRgb(props.value))
const backgroundColor = computed(() => image_rgbToString(rgbValue.value))
const textColor = computed(() =>
  image_rgbToString(image_rgbPseudoComplement(rgbValue.value)),
)
const displayText = computed(() => image_hsvToString(props.value))
</script>

<template>
  <div
    :style="{
      color: textColor,
      backgroundColor: backgroundColor,
      width: 'fit-content',
      border: '1px solid black',
      padding: '0.25em',
    }"
  >
    {{ displayText }}
  </div>
</template>
