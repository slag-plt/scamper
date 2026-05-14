<script setup lang="ts">
import { ASTArg } from "../ast"
import CodeElement from "../../lpm/renderers/vue/components/CodeElement.vue"
import ValueRenderer from "../../lpm/renderers/vue/ValueRenderer.vue"

const { args } = defineProps<{ args: ASTArg[] }>()
if (args.length <= 0) console.warn("!! code parens args is null?", args)
</script>

<!-- if we ever pass in an empty args array to CodeParens, nothing would show up.
      if this is ever an issue, this is where it would be. -->
<template v-if="args.length > 0">
  <CodeElement>(</CodeElement>
  <CodeElement v-if="typeof args[0] === 'string'">{{ args[0] }}</CodeElement>
  <ValueRenderer v-else :value="args[0]" />
  <template v-for="(arg, index) in args.slice(1)" :key="index">
    <CodeElement>{{ " " }}</CodeElement>
    <template v-if="typeof arg === 'string'">{{ arg }}</template>
    <ValueRenderer v-else :value="arg" />
  </template>
  <CodeElement>)</CodeElement>
</template>

<style scoped></style>
