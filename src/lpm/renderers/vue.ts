import { Renderer } from "./index"
import { Component, defineComponent, h, PropType } from "vue"
import { isArray, isList, isPair, isStruct, Value } from "../index"
import CodeElement from "./vue/components/CodeElement.vue"
import VectorRenderer from "./vue/components/VectorRenderer.vue"
import ListRenderer from "./vue/components/ListRenderer.vue"
import PairRenderer from "./vue/components/PairRenderer.vue"
import { simpleRenderers } from "./vue/simple-renderers"
import StructRenderer from "./vue/components/StructRenderer.vue"
import DOMElementRenderer from "./vue/components/DOMElementRenderer.vue"

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

const htmlElementStrategy: DOMStrategy = {
  predicate: (v) => v instanceof HTMLElement,
  type: "dom",
}

const standardStrategies: Strategy[] = [
  ...simpleRenderers,
  vectorStrategy,
  listStrategy,
  pairStrategy,
  htmlElementStrategy,
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
  getStrategy(value: Value): Strategy | undefined {
    return standardStrategies.find((s) => s.predicate(value))
  }

  render(value: Value): Component {
    const strategy = this.getStrategy(value)
    if (strategy) {
      return strategy.type === "vue" ? strategy.renderer : DOMElementRenderer
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
