<script setup lang="ts">
import { HljsBindings } from "../ast"
import CodeElement from "../../lpm/renderers/vue/components/CodeElement.vue"
import ValueRenderer from "../../lpm/renderers/vue/ValueRenderer.vue"

defineProps<{ bindings: HljsBindings }>()
</script>
<!-- TODO: this looks weird, fix the formatting later-->
<template class="hljs">
  <CodeElement>{{ `(${bindings.head} ` }}</CodeElement>
  <template v-if="bindings.scrutinee">
    <ValueRenderer :value="bindings.scrutinee" />
  </template>
  <template v-for="(pair, idx) in bindings.pairs" :key="idx">
    <CodeElement>{{ " [" }}</CodeElement>
    <template v-if="typeof pair.lhs === 'string'">
      {{ pair.lhs }}
    </template>
    <ValueRenderer v-else :value="pair.lhs" />
    <CodeElement>{{ " " }}</CodeElement>
    <ValueRenderer :value="pair.rhs" />
    <CodeElement>{{ "]" }}</CodeElement>
  </template>
  <template v-if="bindings.body">
    <CodeElement>{{ " " }}</CodeElement>
    <ValueRenderer :value="bindings.body" />
  </template>
  <CodeElement>{{ ")" }}</CodeElement>
</template>

<style scoped></style>
