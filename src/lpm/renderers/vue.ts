import { Renderer } from './index'
import { Component, defineComponent, h, PropType } from 'vue'
import CodeElement from './vue/components/CodeElement.vue'
import VectorRenderer from './vue/components/VectorRenderer.vue'
import ListRenderer from './vue/components/ListRenderer.vue'
import PairRenderer from './vue/components/PairRenderer.vue'
import { simpleRenderers } from './vue/simple-renderers'
import StructRenderer from './vue/components/StructRenderer.vue'
import ObjRenderer from './vue/components/ObjRenderer.vue'
import DOMElementRenderer from './vue/components/DOMElementRenderer.vue'
import ErrorRenderer from './vue/components/ErrorRenderer.vue'
import { Value } from '../lang'
import { isArray, isList, isObj, isPair, isStruct } from '../util'

export interface VueStrategyProps {
  type: 'vue'
  renderer: Component
}
interface BaseStrategy {
  predicate: (v: Value) => boolean
}
interface VueStrategy extends BaseStrategy, VueStrategyProps {}
interface DOMStrategy extends BaseStrategy {
  type: 'dom'
}
export type Strategy = VueStrategy | DOMStrategy

/**
 * helper functions
 */
// `T` is what the caller's `formatFn` takes, and it is carried into the
// component's `value` prop so the two cannot drift. The rule counts only
// its appearances in the signature, where it shows up once.
// eslint-disable-next-line @typescript-eslint/no-unnecessary-type-parameters
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
  type: 'vue',
  renderer: VectorRenderer,
}
const listStrategy: VueStrategy = {
  predicate: (v) => isList(v),
  type: 'vue',
  renderer: ListRenderer,
}
const pairStrategy: VueStrategy = {
  predicate: (v) => isPair(v),
  type: 'vue',
  renderer: PairRenderer,
}

const htmlElementStrategy: DOMStrategy = {
  predicate: (v) => v instanceof HTMLElement,
  type: 'dom',
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
  type: 'vue',
  renderer: StructRenderer,
}
const errorStrategy: VueStrategy = {
  predicate: (v) => v instanceof Error,
  type: 'vue',
  renderer: ErrorRenderer,
}
// A map value. Deliberately checked *after* the custom renderers, alongside the
// struct fallback: a library that registers a renderer for its own plain-object
// value still wins over the generic map rendering.
const objStrategy: VueStrategy = {
  predicate: (v) => isObj(v),
  type: 'vue',
  renderer: ObjRenderer,
}

class _VueRenderer extends Renderer<Component> {
  getStrategy(value: Value): Strategy | undefined {
    return standardStrategies.find((s) => s.predicate(value))
  }

  render(value: Value): Component {
    const strategy = this.getStrategy(value)
    if (strategy) {
      return strategy.type === 'vue' ? strategy.renderer : DOMElementRenderer
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
    if (objStrategy.predicate(value)) {
      return objStrategy.renderer
    }
    console.warn('no renderer for', value)
    return FallbackRenderer
  }
}

const VueRenderer = new _VueRenderer()
export default VueRenderer
