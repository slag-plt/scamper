<script setup lang="ts">
import { ASTArg } from "../ast"
import CodeElement from "../../web/output/renderers/components/CodeElement.vue"
import ValueRenderer from "../../web/output/renderers/ValueRenderer.vue"

defineProps<{ args: ASTArg[] }>()
</script>

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
