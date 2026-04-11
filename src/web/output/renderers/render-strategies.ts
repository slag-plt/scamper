import { isArray, isSym, Sym, Value } from "../../../lpm"
import { Component, defineComponent, h, PropType } from "vue"
import CodeElement from "./CodeElement.vue"
import VectorRenderer from "./VectorRenderer.vue"

interface VueRenderProps {
  type: "vue"
  renderer: Component
}
export type Strategy = {
  predicate: (v: Value) => boolean
} & (VueRenderProps | { type: "dom"; renderFn: (v: Value) => HTMLElement })

/**
 * helper functions
 */
function createTextRenderer<T>(formatFn: (val: T) => string): Component {
  return defineComponent({
    props: { value: { type: null as unknown as PropType<T>, required: true } },
    setup(props) {
      return () => h(CodeElement, () => formatFn(props.value as T))
    },
  })
}
export function createSimpleVueRenderer<T>(
  formatFn: (val: T) => string,
): VueRenderProps {
  return {
    type: "vue",
    renderer: createTextRenderer<T>(formatFn),
  }
}

/**
 * strategies
 */
const booleanStrategy: Strategy = {
  predicate: (v) => v === true || v === false,
  ...createSimpleVueRenderer<boolean>((v) => (v ? "#t" : "#f")),
}
const numberStrategy: Strategy = {
  predicate: (v) => typeof v === "number",
  ...createSimpleVueRenderer<number>((v) => v.toString()),
}
const stringStrategy: Strategy = {
  predicate: (v) => typeof v === "string",
  ...createSimpleVueRenderer<string>((v) => `"${v}"`),
}
const undefinedStrategy: Strategy = {
  predicate: (v) => v === undefined,
  ...createSimpleVueRenderer<null | undefined>(() => "void"),
}
const nullStrategy: Strategy = {
  predicate: (v) => v === null,
  ...createSimpleVueRenderer<null>(() => "()"),
}
const symbolStrategy: Strategy = {
  predicate: (v) => isSym(v),
  ...createSimpleVueRenderer<Sym>((v) => v.value),
}
const vectorStrategy: Strategy = {
  predicate: (v) => isArray(v),
  type: "vue",
  renderer: VectorRenderer,
}

export const renderStrategies: Strategy[] = [
  booleanStrategy,
  numberStrategy,
  stringStrategy,
  undefinedStrategy,
  nullStrategy,
  symbolStrategy,
  vectorStrategy,
]
