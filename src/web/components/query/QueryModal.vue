<script setup lang="ts">
import { onMounted, onUnmounted, ref } from "vue"
import { useEditor } from "../editor-context"
import type { PopupCoords } from "./query-modal-extension"

const props = defineProps<{ targetPos: number }>()
defineEmits<{ close: [] }>()

const editor = useEditor()
const coords = ref<PopupCoords | null>(null)
let unsubscribe: (() => void) | null = null

function recompute() {
  coords.value = editor().coordsAtPos(props.targetPos)
}

onMounted(() => {
  unsubscribe = editor().onViewChange(recompute)
  recompute()
})

onUnmounted(() => {
  unsubscribe?.()
  unsubscribe = null
})
</script>

<template>
  <Teleport to="body">
    <div
      v-if="coords"
      class="query-modal"
      :style="{
        position: 'fixed',
        top: coords.bottom + 'px',
        left: coords.left + 'px',
      }"
    >
      <button class="query-modal__close" @click="$emit('close')">x</button>
      <div class="query-modal__content">
        <slot />
      </div>
    </div>
  </Teleport>
</template>

<style scoped>
.query-modal {
  z-index: 1000;
  max-width: 24rem;
  padding: 0.5rem 0.75rem;
  background-color: #fefefe;
  border: 1px solid #888;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
}

.query-modal__close {
  float: right;
  border: none;
  background: none;
  cursor: pointer;
  font-size: 0.875rem;
  line-height: 1;
  padding: 0;
  margin-left: 0.5rem;
}

.query-modal__content {
  font-family:
    Menlo, Consolas, Monaco, "Liberation Mono", "Lucida Console", monospace;
  font-size: 0.875rem;
  white-space: pre-wrap;
}
</style>
