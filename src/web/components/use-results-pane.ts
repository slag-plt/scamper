import type { VueDisplay } from "../../lpm/output/vue"

/**
 * Public interface of ResultsPane.vue exposed via defineExpose.
 * Only the genuinely imperative operations are included; isDirty and
 * isTracing are plain props flowing down from IdeApp.
 * Template refs to a child component receive Vue's public instance from
 * getComponentPublicInstance(), which (when defineExpose was used) is an
 * exposeProxy whose target is proxyRefs(exposed). Property reads on that
 * proxy run unref() on each value, so exposed computed refs unwrap to
 * VueDisplay | undefined in both template and parent script (e.g.
 * resultsRef.value.display), not a ComputedRef instance.
 */
export interface ResultsPaneType {
  reset(): void
  scrollToBottom(): void
  readonly display: VueDisplay | undefined
}
