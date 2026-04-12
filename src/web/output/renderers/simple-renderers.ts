import {
  Char,
  charToName,
  Closure,
  isChar,
  isClosure,
  isJsFunction,
  isSym,
  Sym,
} from "../../../lpm"
import { createTextRenderer, Strategy, VueRenderProps } from "./VueRenderer"

export function createSimpleVueRenderer<T>(
  formatFn: (val: T) => string,
): VueRenderProps {
  return {
    type: "vue",
    renderer: createTextRenderer<T>(formatFn),
  }
}
/**
 * simple strategies
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
const closureStrategy: Strategy = {
  predicate: (v) => isClosure(v),
  ...createSimpleVueRenderer(
    (v) =>
      `(lambda (${(v as Closure).params.reduce((acc, curr) => `${acc} ${curr}`)}) ...)`,
  ),
}
const jsFunctionStrategy: Strategy = {
  predicate: (v) => isJsFunction(v),
  ...createSimpleVueRenderer<() => void>(
    (v) => `js.${v.name || "##anonymous##"}`,
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
  symbolStrategy,
  closureStrategy,
  jsFunctionStrategy,
  charStrategy,
]
