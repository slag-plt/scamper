<script setup lang="ts">
import { ref } from 'vue'

const values = ref<unknown[]>([])

const display = {
  send: (value: unknown) => values.value.push(value),
  report: (error: unknown) => values.value.push(error),
  pushLevel: () => undefined,
  popLevel: () => undefined,
  get totalSends() {
    return values.value.length
  },
}

function reset(): void {
  values.value = []
}

function scrollToBottom(): void {
  /* no-op */
}

defineExpose({ display, reset, scrollToBottom })
</script>

<template>
  <div role="status" aria-label="Results">
    <div v-for="(value, index) in values" :key="index">
      {{ value }}
    </div>
  </div>
</template>
