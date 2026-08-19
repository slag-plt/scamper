import type { InjectionKey, Ref } from 'vue'

/**
 * The path to the sub-expression that changed, for whatever is rendering one
 * step of a trace beside the step before it (see src/scheme/layout-diff).
 *
 * Provided rather than passed as a prop because the layout is reached through
 * the generic value renderer -- a trace step is a value, which resolves to a
 * trace renderer, which resolves to an expression renderer -- and threading a
 * path for one caller through all of that would put trace-diffing knowledge
 * into components that have no business with it.
 *
 * Absent outside a trace window, where nothing is being compared.
 */
export const ChangedPathKey: InjectionKey<Ref<number[] | null>> =
  Symbol('ChangedPath')
