<script setup lang="ts">
import { computed } from 'vue'
import { Rgb, color_rgbToString } from '../color'
import ColorSwatch from './ColorSwatch.vue'

const props = defineProps<{ value: Rgb }>()

const label = computed(() => color_rgbToString(props.value))
</script>

<template>
  <ColorSwatch :color="value" :label="label" />
</template>
