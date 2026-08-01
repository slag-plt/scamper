import { computed, ref } from 'vue'

// A Promise-based replacement for the browser's blocking prompt()/confirm()/
// alert() (issue #305). Calling one of the functions below enqueues a modal
// request and resolves once the user responds, so call sites keep reading
// imperatively:
//
//   if (await modalConfirm({ message: 'Delete?' })) { ... }
//
// ModalHost.vue renders the active request via AppModal.vue; place one host in
// the app's root template. This module holds no DOM references, so it is unit
// testable on its own.

export type ModalKind = 'alert' | 'confirm' | 'prompt'

export interface ModalRequest {
  /** A unique, monotonic id; ModalHost keys the dialog on it so each request
   * gets a fresh AppModal instance (and thus a fresh showModal/focus cycle). */
  id: number
  kind: ModalKind
  title?: string
  message: string
  confirmLabel: string
  /** The dismiss action's label; absent for a single-button alert. */
  cancelLabel?: string
  /** Styles the confirm button as a destructive action. */
  danger: boolean
  /** prompt only: initial input value and placeholder. */
  defaultValue?: string
  placeholder?: string
  resolve: (value: boolean | string | null | undefined) => void
}

// Requests are queued so overlapping calls (e.g. a loop that confirms each of
// several files) each get their turn rather than clobbering one another. The
// active request is always the head of the queue.
const queue = ref<ModalRequest[]>([])

let nextId = 0

export const activeModal = computed<ModalRequest | null>(() =>
  queue.value.length > 0 ? queue.value[0] : null,
)

function enqueue(request: Omit<ModalRequest, 'id'>): void {
  queue.value = [...queue.value, { ...request, id: nextId++ }]
}

function dequeue(): ModalRequest | null {
  if (queue.value.length === 0) return null
  const current = queue.value[0]
  queue.value = queue.value.slice(1)
  return current
}

/** Resolves the active modal with the confirm action's value. */
export function resolveModal(value: boolean | string | null | undefined): void {
  dequeue()?.resolve(value)
}

/** Resolves the active modal with its dismissal value (Esc / backdrop / cancel). */
export function dismissModal(): void {
  const current = dequeue()
  if (current === null) return
  current.resolve(current.kind === 'prompt' ? null : false)
}

export interface AlertOptions {
  message: string
  title?: string
  confirmLabel?: string
}

/** Shows a single-button informational modal. Resolves when dismissed. */
export function modalAlert(opts: AlertOptions): Promise<void> {
  return new Promise((resolve) => {
    enqueue({
      kind: 'alert',
      title: opts.title,
      message: opts.message,
      confirmLabel: opts.confirmLabel ?? 'OK',
      danger: false,
      resolve: () => {
        resolve()
      },
    })
  })
}

export interface ConfirmOptions {
  message: string
  title?: string
  confirmLabel?: string
  cancelLabel?: string
  danger?: boolean
}

/** Shows a confirm/cancel modal. Resolves true on confirm, false otherwise. */
export function modalConfirm(opts: ConfirmOptions): Promise<boolean> {
  return new Promise((resolve) => {
    enqueue({
      kind: 'confirm',
      title: opts.title,
      message: opts.message,
      confirmLabel: opts.confirmLabel ?? 'OK',
      cancelLabel: opts.cancelLabel ?? 'Cancel',
      danger: opts.danger ?? false,
      resolve: (value) => {
        resolve(value === true)
      },
    })
  })
}

export interface PromptOptions {
  message: string
  title?: string
  defaultValue?: string
  placeholder?: string
  confirmLabel?: string
  cancelLabel?: string
}

/** Shows a text-input modal. Resolves the entered string, or null if cancelled. */
export function modalPrompt(opts: PromptOptions): Promise<string | null> {
  return new Promise((resolve) => {
    enqueue({
      kind: 'prompt',
      title: opts.title,
      message: opts.message,
      confirmLabel: opts.confirmLabel ?? 'OK',
      cancelLabel: opts.cancelLabel ?? 'Cancel',
      danger: false,
      defaultValue: opts.defaultValue,
      placeholder: opts.placeholder,
      resolve: (value) => {
        resolve(typeof value === 'string' ? value : null)
      },
    })
  })
}
