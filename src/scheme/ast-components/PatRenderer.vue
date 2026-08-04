<script setup lang="ts">
import { computed } from 'vue'
import { Pat, patToLayout } from '../ast'
import LayoutRenderer from './LayoutRenderer.vue'

const props = defineProps<{ value: Pat }>()
const layout = computed(() => patToLayout(props.value))
</script>

<template>
  <LayoutRenderer :layout="layout" />
</template>

<style scoped></style>
