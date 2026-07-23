<script setup lang="ts">
import { computed } from 'vue'
import { Rgb, image_rgbToString, image_rgbPseudoComplement } from '../color'

const props = defineProps<{ value: Rgb }>()

const backgroundColor = computed(() => image_rgbToString(props.value))
const textColor = computed(() => image_rgbToString(image_rgbPseudoComplement(props.value)))
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
