<script setup lang="ts">
import { Pat, PCtor, PVar } from "../ast"
import { createSimpleVueRenderer } from "../../web/output/renderers/simple-renderers"
import ValueRenderer from "../../web/output/renderers/ValueRenderer.vue"
import CodeParens from "./CodeParens.vue"
import { FallbackRenderer } from "../../web/output/renderers/vue"

const { value: pat } = defineProps<{ value: Pat }>()

let computedComponent: any
switch (pat.tag) {
  case "pwild":
    computedComponent = createSimpleVueRenderer(() => "_").renderer
    break
  case "pvar":
    computedComponent = createSimpleVueRenderer<PVar>((pat) => pat.name).renderer
    break
  case "plit":
    computedComponent = ValueRenderer
    break
  case "pctor": {
    if (pat.args.length === 0) {
      computedComponent = createSimpleVueRenderer<PCtor>((pat) => `(${pat.name})`).renderer
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
  <component
    :is="computedComponent"
    v-if="computedComponent !== CodeParens"
    :value="pat"
  />
  <CodeParens v-else :args="(pat as PCtor).args" />
</template>

<style scoped></style>
