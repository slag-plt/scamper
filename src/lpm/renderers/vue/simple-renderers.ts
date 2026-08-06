import { Char, Closure } from '../../lang'
import {
  charToName,
  escapeStringLiteral,
  isChar,
  isClosure,
  isJsFunction,
} from '../../util'
import { createTextRenderer, Strategy, VueStrategyProps } from '../vue'

export function createSimpleVueRenderer<T>(
  formatFn: (val: T) => string,
): VueStrategyProps {
  return {
    type: 'vue',
    renderer: createTextRenderer<T>(formatFn),
  }
}
/**
 * simple strategies
 */
const booleanStrategy: Strategy = {
  predicate: (v) => v === true || v === false,
  ...createSimpleVueRenderer<boolean>((v) => (v ? '#t' : '#f')),
}
const numberStrategy: Strategy = {
  predicate: (v) => typeof v === 'number',
  ...createSimpleVueRenderer<number>((v) => v.toString()),
}
const stringStrategy: Strategy = {
  predicate: (v) => typeof v === 'string',
  ...createSimpleVueRenderer<string>((v) => `"${escapeStringLiteral(v)}"`),
}
const undefinedStrategy: Strategy = {
  predicate: (v) => v === undefined,
  ...createSimpleVueRenderer<null | undefined>(() => 'void'),
}
const nullStrategy: Strategy = {
  predicate: (v) => v === null,
  // Render the empty list / null value as `null`, matching TextRenderer (and the
  // CLI). The web renderer previously diverged, showing `()`.
  ...createSimpleVueRenderer<null>(() => 'null'),
}
const closureStrategy: Strategy = {
  predicate: (v) => isClosure(v),
  ...createSimpleVueRenderer<Closure>((v) => {
    // Rest parameters use Clojure-style "&", e.g. (lambda (x & xs) ...) and the
    // rest-only (lambda (& xs) ...).
    const params = [...v.params]
    if (v.restParam) params.push('&', v.restParam)
    return `(lambda (${params.join(' ')}) ...)`
  }),
}
const jsFunctionStrategy: Strategy = {
  predicate: (v) => isJsFunction(v),
  ...createSimpleVueRenderer<() => void>(
    (v) => `js.${v.name || '##anonymous##'}`,
  ),
}
const charStrategy: Strategy = {
  predicate: (v) => isChar(v),
  ...createSimpleVueRenderer<Char>((v) => `#\\${charToName(v.value)}`),
}

export const simpleRenderers: Strategy[] = [
  booleanStrategy,
  numberStrategy,
  stringStrategy,
  undefinedStrategy,
  nullStrategy,
  closureStrategy,
  jsFunctionStrategy,
  charStrategy,
]
