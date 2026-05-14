<script setup lang="ts">
import { computed } from "vue"
import {
  Hsv,
  hsvToRgb,
  hsvToString,
  rgbToString,
  rgbPseudoComplement,
} from "./color"

const props = defineProps<{ value: Hsv }>()

const rgbValue = computed(() => hsvToRgb(props.value))
const backgroundColor = computed(() => rgbToString(rgbValue.value))
const textColor = computed(() =>
  rgbToString(rgbPseudoComplement(rgbValue.value)),
)
const displayText = computed(() => hsvToString(props.value))
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
