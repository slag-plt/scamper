<script setup lang="ts">
import { ref } from "vue"

const emit = defineEmits<{ dirty: [] }>()
const src = ref("")

function getDoc(): string {
  return src.value
}

function initializeDoc(nextSrc: string): void {
  src.value = nextSrc
}

function initializeDummyDoc(): void {
  src.value = ""
}

function onInput(event: Event): void {
  src.value = (event.target as HTMLTextAreaElement).value
  emit("dirty")
}

defineExpose({ getDoc, initializeDoc, initializeDummyDoc })
</script>

<template>
  <textarea aria-label="Source code" :value="src" @input="onInput" />
</template>
