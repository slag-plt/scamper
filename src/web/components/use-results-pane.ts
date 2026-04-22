import type { VueDisplay } from "../../lpm/output/vue"

/**
 * Public interface of ResultsPane.vue exposed via defineExpose.
 * Only the genuinely imperative operations are included; isDirty and
 * isTracing are plain props flowing down from IdeApp.
 * Vue auto-unwraps computed refs accessed through template refs, so
 * display is typed as VueDisplay | undefined rather than ComputedRef.
 */
export interface ResultsPaneType {
  reset(): void
  scrollToBottom(): void
  readonly display: VueDisplay | undefined
}
