import { Renderer } from "../../../lpm/renderers"
import { Component, defineComponent, h, PropType } from "vue"
import { isArray, isList, isPair, Value } from "../../../lpm"
import CodeElement from "./components/CodeElement.vue"
import VectorRenderer from "./components/VectorRenderer.vue"
import ListRenderer from "./components/ListRenderer.vue"
import PairRenderer from "./components/PairRenderer.vue"
import { simpleRenderers } from "./simple-renderers"

export interface VueRenderProps {
  type: "vue"
  renderer: Component
}
export type Strategy = {
  predicate: (v: Value) => boolean
} & (VueRenderProps | { type: "dom"; renderFn: (v: Value) => HTMLElement })

/**
 * helper functions
 */
export function createTextRenderer<T>(formatFn: (val: T) => string): Component {
  return defineComponent({
    props: { value: { type: null as unknown as PropType<T>, required: true } },
    setup(props) {
      return () => h(CodeElement, () => formatFn(props.value as T))
    },
  })
}

export const FallbackRenderer = createTextRenderer(
  (v) => `[Blob: ${JSON.stringify(v)}]`,
)

/**
 * non-trivial vue strategies
 */
const vectorStrategy: Strategy = {
  predicate: (v) => isArray(v),
  type: "vue",
  renderer: VectorRenderer,
}
const listStrategy: Strategy = {
  predicate: (v) => isList(v),
  type: "vue",
  renderer: ListRenderer,
}
const pairStrategy: Strategy = {
  predicate: (v) => isPair(v),
  type: "vue",
  renderer: PairRenderer,
}

const standardStrategies: Strategy[] = [
  ...simpleRenderers,
  vectorStrategy,
  listStrategy,
  pairStrategy,
]

class _VueRenderer extends Renderer<Component> {
  render(value: Value): Component {
    const strategy = standardStrategies.find((s) => s.predicate(value))

    if (strategy) {
      if (strategy.type === "vue") return strategy.renderer
      // TODO: implement DOM type renderer
      return FallbackRenderer
    }

    // there may be a custom renderer for this value
    const customRenderer = this.getCustomRendererFor(value)
    if (!customRenderer) return FallbackRenderer
    return customRenderer(value)
  }
}

const VueRenderer = new _VueRenderer()
export default VueRenderer
