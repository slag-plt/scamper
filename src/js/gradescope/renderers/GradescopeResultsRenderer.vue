<script setup lang="ts">
import * as L from '../../../lpm'
import { toJsonText } from './json'

defineProps<{ value: L.Value }>()
</script>

<template>
  <pre class="gradescope-results">{{ toJsonText(value) }}</pre>
</template>

<style scoped>
.gradescope-results {
  white-space: pre-wrap;
  word-break: break-word;
}
</style>
