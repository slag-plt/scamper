<script setup lang="ts">
import { nextTick, ref, watch } from 'vue'
import AppModal from './AppModal.vue'
import {
  activeModal,
  dismissModal,
  resolveModal,
} from '../composables/use-modals'

// Renders whichever modal request is currently active (see use-modals.ts) using
// the generic AppModal. Drop a single <ModalHost /> into the app root; the
// imperative modalAlert/modalConfirm/modalPrompt helpers drive it.

const inputValue = ref('')
const inputRef = ref<HTMLInputElement | null>(null)

// When a prompt becomes active, seed its input and focus it.
watch(activeModal, async (modal) => {
  if (modal?.kind !== 'prompt') return
  inputValue.value = modal.defaultValue ?? ''
  await nextTick()
  inputRef.value?.focus()
  inputRef.value?.select()
})

function onConfirm() {
  const modal = activeModal.value
  if (modal === null) return
  if (modal.kind === 'prompt') {
    resolveModal(inputValue.value)
  } else if (modal.kind === 'confirm') {
    resolveModal(true)
  } else {
    resolveModal(undefined)
  }
}

function onCancel() {
  dismissModal()
}
</script>

<template>
  <AppModal
    :key="activeModal?.id ?? -1"
    :open="activeModal !== null"
    :title="activeModal?.title"
    @dismiss="onCancel"
  >
    <p class="modal-message">{{ activeModal?.message }}</p>
    <form
      v-if="activeModal?.kind === 'prompt'"
      class="modal-form"
      @submit.prevent="onConfirm"
    >
      <input
        ref="inputRef"
        v-model="inputValue"
        class="modal-input"
        type="text"
        :aria-label="activeModal?.message"
        :placeholder="activeModal?.placeholder"
      />
    </form>
    <template #footer>
      <button
        v-if="activeModal?.cancelLabel"
        type="button"
        class="modal-button"
        @click="onCancel"
      >
        {{ activeModal?.cancelLabel }}
      </button>
      <button
        type="button"
        class="modal-button modal-button--primary"
        :class="{ 'modal-button--danger': activeModal?.danger }"
        :autofocus="activeModal?.kind !== 'prompt'"
        @click="onConfirm"
      >
        {{ activeModal?.confirmLabel }}
      </button>
    </template>
  </AppModal>
</template>

<style scoped>
.modal-message {
  margin: 0;
  white-space: pre-wrap;
  overflow-wrap: anywhere;
}

.modal-form {
  margin: 0;
}

.modal-input {
  width: 100%;
  box-sizing: border-box;
  padding: 0.4rem 0.5rem;
  border: 1px solid var(--border);
  border-radius: 6px;
  background-color: var(--surface);
  color: var(--fg);
  font: inherit;
}

.modal-input:focus {
  outline: 2px solid var(--accent);
  outline-offset: -1px;
}

.modal-button {
  padding: 0.4rem 0.9rem;
  border: 1px solid var(--border);
  border-radius: 6px;
  background-color: var(--surface);
  color: var(--fg);
  font: inherit;
  cursor: pointer;
}

.modal-button:hover {
  background-color: var(--surface-hover);
}

.modal-button--primary {
  border-color: var(--accent);
  background-color: var(--accent);
  color: var(--accent-fg);
}

.modal-button--primary:hover {
  filter: brightness(1.05);
}

.modal-button--danger {
  border-color: var(--danger);
  background-color: var(--danger);
  color: var(--danger-fg);
}
</style>
