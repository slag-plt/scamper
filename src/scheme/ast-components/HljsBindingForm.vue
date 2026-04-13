<script setup lang="ts">
import { HljsBindings } from "../ast"
import CodeElement from "../../lpm/renderers/vue/components/CodeElement.vue"
import ValueRenderer from "../../lpm/renderers/vue/ValueRenderer.vue"

defineProps<{ bindings: HljsBindings }>()
</script>
<!-- TODO: this looks weird, fix the formatting later-->
<template>
  <CodeElement class="hljs">
    {{ "(" + bindings.head }}
    <template v-if="bindings.scrutinee">
      {{ " " }}
      <ValueRenderer :value="bindings.scrutinee" />
    </template>
    <template v-for="(pair, idx) in bindings.pairs" :key="idx">
      {{ " [" }}
      <template v-if="typeof pair.lhs === 'string'">
        {{ pair.lhs }}
      </template>
      <ValueRenderer v-else :value="pair.lhs" />
      {{ " " }}<ValueRenderer :value="pair.rhs" />{{ "]" }}
    </template>
    <template v-if="bindings.body">
      {{ " " }}<ValueRenderer :value="bindings.body" />
    </template>
    {{ ")" }}
  </CodeElement>
</template>

<style scoped></style>
