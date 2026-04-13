<script setup lang="ts">
import { Pat, PCtor, PVar } from "../ast"
import { createSimpleVueRenderer } from "../../lpm/renderers/vue/simple-renderers"
import ValueRenderer from "../../lpm/renderers/vue/ValueRenderer.vue"
import CodeParens from "./CodeParens.vue"
import { FallbackRenderer } from "../../lpm/renderers/vue"

const { value: pat } = defineProps<{ value: Pat }>()

let computedComponent: any
switch (pat.tag) {
  case "pwild":
    computedComponent = createSimpleVueRenderer(() => "_").renderer
    break
  case "pvar":
    computedComponent = createSimpleVueRenderer<PVar>(
      (pat) => pat.name,
    ).renderer
    break
  case "plit":
    computedComponent = ValueRenderer
    break
  case "pctor": {
    if (pat.args.length === 0) {
      computedComponent = createSimpleVueRenderer<PCtor>(
        (pat) => `(${pat.name})`,
      ).renderer
    } else {
      computedComponent = CodeParens
    }
    break
  }
  default:
    computedComponent = FallbackRenderer
}
</script>

<template>
  <ValueRenderer v-if="pat.tag === 'plit'" :value="pat.value" />
  <CodeParens
    v-else-if="pat.tag === 'pctor' && pat.args.length > 0"
    :args="[pat.name, ...pat.args]"
  />
  <component v-else :is="computedComponent" :value="pat" />
</template>

<style scoped></style>
