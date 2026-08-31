<script setup lang="ts">
import { PropType } from 'vue'
import * as L from '../../../lpm'
import { toJsonText } from './json'

// Runtime-declared for the reason ValueRenderer's `value` is: a type
// declaration naming `Value` puts `Boolean` in the prop's runtime type, and
// Vue then casts an incoming empty string to `true` (#444).
defineProps({
  value: { type: null as unknown as PropType<L.Value>, required: true },
})
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
