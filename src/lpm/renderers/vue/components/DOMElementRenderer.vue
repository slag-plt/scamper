<script setup lang="ts">
import { ref, onMounted, watch } from "vue"

const props = defineProps<{
  value: HTMLElement
}>()

const container = ref<HTMLElement | null>(null)

function updateElement() {
  if (container.value) {
    container.value.replaceChildren(props.value)
  }
}

onMounted(() => {
  updateElement()
})

watch(
  () => props.value,
  () => {
    updateElement()
  },
)
</script>

<template>
  <span ref="container" class="scamper-dom-element"></span>
</template>

<style scoped>
.scamper-dom-element {
  display: contents;
}
</style>
