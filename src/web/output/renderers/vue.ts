import { Renderer } from "../../../lpm/renderers"
import { Component, defineComponent, h, PropType } from "vue"
import { isArray, isList, isPair, isStruct, Value } from "../../../lpm"
import CodeElement from "./components/CodeElement.vue"
import VectorRenderer from "./components/VectorRenderer.vue"
import ListRenderer from "./components/ListRenderer.vue"
import PairRenderer from "./components/PairRenderer.vue"
import { simpleRenderers } from "./simple-renderers"
import StructRenderer from "./components/StructRenderer.vue"

export interface VueStrategyProps {
  type: "vue"
  renderer: Component
}
interface BaseStrategy {
  predicate: (v: Value) => boolean
}
interface VueStrategy extends BaseStrategy, VueStrategyProps {}
interface DOMStrategy extends BaseStrategy {
  type: "dom"
  renderFn: (v: Value) => HTMLElement
}
export type Strategy = VueStrategy | DOMStrategy

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
const vectorStrategy: VueStrategy = {
  predicate: (v) => isArray(v),
  type: "vue",
  renderer: VectorRenderer,
}
const listStrategy: VueStrategy = {
  predicate: (v) => isList(v),
  type: "vue",
  renderer: ListRenderer,
}
const pairStrategy: VueStrategy = {
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

const genericStructStrategy: VueStrategy = {
  predicate: (v) => isStruct(v),
  type: "vue",
  renderer: StructRenderer,
}
const errorStrategy: VueStrategy = {
  predicate: (v) => v instanceof Error,
  type: "vue",
  renderer: createTextRenderer<Error>((v) => v.toString()),
}

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
    if (customRenderer) {
      return customRenderer(value)
    }

    // otherwise, it can either be a struct, error, or we don't have a renderer for it
    if (genericStructStrategy.predicate(value)) {
      return genericStructStrategy.renderer
    }
    if (errorStrategy.predicate(value)) {
      return errorStrategy.renderer
    }
    console.warn("no renderer for", value)
    return FallbackRenderer
  }
}

const VueRenderer = new _VueRenderer()
export default VueRenderer
