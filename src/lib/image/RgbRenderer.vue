<script setup lang="ts">
import { computed } from "vue"
import { Rgb, rgbToString, rgbPseudoComplement } from "./color"

const props = defineProps<{ value: Rgb }>()

const backgroundColor = computed(() => rgbToString(props.value))
const textColor = computed(() => rgbToString(rgbPseudoComplement(props.value)))
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
    {{ backgroundColor }}
  </div>
</template>
