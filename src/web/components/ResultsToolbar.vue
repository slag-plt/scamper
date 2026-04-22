<script setup lang="ts">
import { ref } from "vue"

const props = defineProps<{
  isTracing: boolean
  isDirty: boolean
  stepOnce?: () => void
  stepStmt?: () => Promise<void>
  stepAll?: () => Promise<void>
  astText?: () => void
  cancel?: () => void
}>()

const isSteppingStmt = ref(false)
const isSteppingAll = ref(false)

async function handleStepStmt() {
  isSteppingStmt.value = true
  try {
    await props.stepStmt?.()
  } finally {
    isSteppingStmt.value = false
  }
}

async function handleStepAll() {
  isSteppingAll.value = true
  try {
    await props.stepAll?.()
  } finally {
    isSteppingAll.value = false
  }
}
</script>

<template>
  <div class="results-toolbar">
    <div>
      <button
        class="fa-solid fa-shoe-prints"
        aria-label="Step once"
        :disabled="!isTracing || isSteppingStmt || isSteppingAll"
        @click="stepOnce?.()"
      ></button>

      <template v-if="isSteppingStmt">
        <button
          class="fa-solid fa-stop"
          aria-label="Stop"
          @click="cancel?.()"
        ></button>
        <i class="fa-solid fa-spinner fa-spin"></i>
      </template>
      <button
        v-else
        class="fa-solid fa-forward-step"
        aria-label="Step Statement"
        :disabled="!isTracing || isSteppingAll"
        @click="handleStepStmt"
      ></button>

      <template v-if="isSteppingAll">
        <button
          class="fa-solid fa-stop"
          aria-label="Stop"
          @click="cancel?.()"
        ></button>
        <i class="fa-solid fa-spinner fa-spin"></i>
      </template>
      <button
        v-else
        class="fa-solid fa-forward"
        aria-label="Step All"
        :disabled="!isTracing || isSteppingStmt"
        @click="handleStepAll"
      ></button>

      <button
        class="fa-solid fa-tree"
        aria-label="Show AST"
        disabled
        @click="astText?.()"
      ></button>
    </div>
    <div v-if="isDirty" class="results-status">
      <em>(Warning: results out of sync with updated code)</em>
    </div>
  </div>
</template>

<style scoped>
.results-toolbar > div {
  display: flex;
  align-items: center;
  gap: 0.25em;
}
</style>
