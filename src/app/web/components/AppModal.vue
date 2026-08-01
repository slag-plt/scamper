<script setup lang="ts">
import { computed, onMounted, ref, useId, watch } from 'vue'

// A generic, themed modal dialog built on the native <dialog> element, meant to
// replace the browser's blocking prompt()/confirm()/alert() (issue #305). It is
// purely presentational: the caller owns the `open` state and fills the body /
// footer via slots. See use-modals.ts + ModalHost.vue for the imperative
// alert/confirm/prompt built on top of it.
//
// N.B., showModal()/close() are feature-detected: jsdom (the test environment)
// implements neither, so we fall back to toggling the `open` attribute. Real
// browsers get the full modal treatment -- ::backdrop, focus trapping, and Esc.
const props = withDefaults(
  defineProps<{
    open: boolean
    title?: string
    // When false, Esc and backdrop clicks do not dismiss the dialog.
    dismissable?: boolean
  }>(),
  { title: undefined, dismissable: true },
)

const emit = defineEmits<{
  // A user-initiated dismissal (Esc or backdrop click); never fired for a
  // programmatic close driven by the `open` prop.
  dismiss: []
  'update:open': [value: boolean]
}>()

const dialogRef = ref<HTMLDialogElement | null>(null)

const titleId = useId()
const bodyId = useId()
const labelledBy = computed(() => (props.title ? titleId : undefined))

// Guards the native `close` event so a programmatic close (open -> false) is not
// mistaken for a user dismissal.
let closingFromProp = false

function sync(open: boolean) {
  const el = dialogRef.value
  if (!el) return
  if (open) {
    if (el.open) return
    if (typeof el.showModal === 'function') {
      el.showModal()
    } else {
      el.setAttribute('open', '')
    }
  } else if (typeof el.close === 'function') {
    // close() fires its `close` event asynchronously (a queued task), so the
    // guard must stay set until onClose runs -- it resets it there.
    closingFromProp = true
    el.close()
  } else {
    // The fallback path fires no event, so there is nothing to guard against.
    el.removeAttribute('open')
  }
}

watch(() => props.open, sync)
onMounted(() => {
  if (props.open) sync(true)
})

function onCancel(e: Event) {
  // The native `cancel` event precedes `close` when Esc is pressed. Block it
  // (and the ensuing close) when the dialog is not dismissable.
  if (!props.dismissable) e.preventDefault()
}

function onClose() {
  if (closingFromProp) {
    closingFromProp = false
    return
  }
  emit('update:open', false)
  emit('dismiss')
}

function onBackdropClick(e: MouseEvent) {
  // A click whose target is the <dialog> itself (not its panel) landed on the
  // backdrop. Route it through the same close path as Esc.
  if (!props.dismissable || e.target !== dialogRef.value) return
  const el = dialogRef.value
  if (el && typeof el.close === 'function') {
    el.close()
  } else {
    onClose()
  }
}
</script>

<template>
  <dialog
    ref="dialogRef"
    class="app-modal"
    aria-modal="true"
    :aria-labelledby="labelledBy"
    :aria-describedby="bodyId"
    @cancel="onCancel"
    @close="onClose"
    @click="onBackdropClick"
  >
    <div class="app-modal__panel">
      <h2 v-if="title" :id="titleId" class="app-modal__title">{{ title }}</h2>
      <div :id="bodyId" class="app-modal__body">
        <slot />
      </div>
      <div class="app-modal__footer">
        <slot name="footer" />
      </div>
    </div>
  </dialog>
</template>

<style scoped>
.app-modal {
  padding: 0;
  border: 1px solid var(--modal-border);
  border-radius: 8px;
  background-color: var(--modal-bg);
  color: var(--fg);
  max-width: min(90vw, 32rem);
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
}

.app-modal::backdrop {
  background-color: var(--overlay);
}

.app-modal__panel {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  padding: 1.25rem;
  min-width: 18rem;
}

.app-modal__title {
  margin: 0;
  font-size: 1.1rem;
  font-weight: 600;
}

.app-modal__body {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
}

.app-modal__footer {
  display: flex;
  justify-content: flex-end;
  gap: 0.5rem;
}
</style>
