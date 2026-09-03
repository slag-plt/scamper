<script setup lang="ts">
import { computed } from 'vue'
import { Hsv, color_hsvToRgb, color_hsvToString } from '../color'
import ColorSwatch from './ColorSwatch.vue'

const props = defineProps<{ value: Hsv }>()

const rgbValue = computed(() => color_hsvToRgb(props.value))
const label = computed(() => color_hsvToString(props.value))
</script>

<template>
  <ColorSwatch :color="rgbValue" :label="label" />
</template>
