<script setup lang="ts">
import { computed } from 'vue'
import { Exp, expToLayout } from '../ast'
import LayoutRenderer from './LayoutRenderer.vue'

const props = defineProps<{ value: Exp }>()
const layout = computed(() => expToLayout(props.value))
</script>

<template>
  <LayoutRenderer :layout="layout" />
</template>

<style scoped></style>
