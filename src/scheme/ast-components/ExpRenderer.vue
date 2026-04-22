<script setup lang="ts">
import { ASTArg, Exp, HljsBindings, mkLit } from "../ast"
import ValueRenderer from "../../lpm/renderers/vue/ValueRenderer.vue"
import CodeParens from "./CodeParens.vue"
import HljsBindingForm from "./HljsBindingForm.vue"
import CodeElement from "../../lpm/renderers/vue/components/CodeElement.vue"

const { value: e } = defineProps<{ value: Exp }>()

let args: ASTArg[] | null = null
switch (e.tag) {
  case "app":
    args = [e.head, ...e.args]
    break
  case "lam":
    args = ["lambda", ...e.params, e.body]
    break
  case "begin":
    args = ["begin", ...e.exps]
    break
  case "if":
    args = ["if", e.guard, e.ifB, e.elseB]
    break
  case "quote":
    args = ["quote", mkLit(e.value)]
    break
  case "and":
    args = ["and", ...e.exps]
    break
  case "or":
    args = ["or", ...e.exps]
    break
  case "section":
    args = ["section", ...e.exps]
    break
}

let hljsBindings: HljsBindings | null = null
switch (e.tag) {
  case "let":
    hljsBindings = {
      head: "let",
      pairs: e.bindings.map(({ name, value }) => ({ lhs: name, rhs: value })),
      body: e.body,
    }
    break
  case "match":
    hljsBindings = {
      head: "match",
      pairs: e.branches.map(({ pat, body }) => ({ lhs: pat, rhs: body })),
      scrutinee: e.scrutinee,
    }
    break
  case "let*":
    hljsBindings = {
      head: "let*",
      pairs: e.bindings.map(({ name, value }) => ({ lhs: name, rhs: value })),
      body: e.body,
    }
    break
  case "cond":
    hljsBindings = {
      head: "cond",
      pairs: e.branches.map(({ test, body }) => ({ lhs: test, rhs: body })),
    }
    break
}
</script>

<template>
  <CodeElement v-if="e.tag === 'var'">
    {{ e.name }}
  </CodeElement>
  <ValueRenderer v-else-if="e.tag === 'lit'" :value="e.value" />
  <CodeParens v-else-if="args !== null" :args="args" />
  <HljsBindingForm v-else-if="hljsBindings !== null" :bindings="hljsBindings" />
</template>

<style scoped></style>
