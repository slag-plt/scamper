<script setup lang="ts">
import { onMounted, onUnmounted, ref } from "vue"
import { ReportError } from "../../../lpm"
import ValueRenderer from "../../../lpm/renderers/vue/ValueRenderer.vue"
import { useEditor } from "../editor-context"
import type { PopupCoords } from "./query-modal-extension"
import { QueryEntry } from "../use-scamper-session"

const props = defineProps<{ query: QueryEntry }>()
defineEmits<{ close: [] }>()

const editor = useEditor()
const coords = ref<PopupCoords | null>(null)
let unsubscribe: (() => void) | null = null
const measureKey = {}

function recompute() {
  editor().requestCoordsAtPos(
    props.query.targetPos,
    (next) => {
      coords.value = next
    },
    measureKey,
  )
}

const isDone = ref(false)

onMounted(() => {
  void props.query.done.finally(() => (isDone.value = true))
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
        <template v-if="!isDone"> Query pending... </template>
        <template v-else-if="query.err.errors.length === 0">
          Queried code could not be reached!
        </template>
        <template
          v-else-if="
            query.err.errors.filter((e) => e instanceof ReportError).length ===
            0
          "
        >
          {{ query.err.errors[0].toString() }}
        </template>
        <ValueRenderer
          v-for="[repI, repErr] in query.err.errors
            .filter((e) => e instanceof ReportError)
            .entries()"
          v-else
          :key="repI"
          :value="repErr.value"
        />
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
