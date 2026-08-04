<script setup lang="ts">
import { HljsBindings } from '../ast'
import CodeElement from '../../lpm/renderers/vue/components/CodeElement.vue'
import ValueRenderer from '../../lpm/renderers/vue/ValueRenderer.vue'

defineProps<{ bindings: HljsBindings }>()
</script>
<!-- Renders let/match/cond so the text matches expToString (#318): a single
     space after the head, `let`'s binding list wrapped in its own parens
     (match/cond clauses are not), and single spaces between clauses. -->
<template>
  <div class="hljs">
    <CodeElement>{{ `(${bindings.head}` }}</CodeElement>
    <template v-if="bindings.scrutinee">
      <CodeElement>{{ " " }}</CodeElement>
      <ValueRenderer :value="bindings.scrutinee" />
    </template>
    <CodeElement>{{ bindings.parenthesizePairs ? " (" : " " }}</CodeElement>
    <template v-for="(pair, idx) in bindings.pairs" :key="idx">
      <CodeElement v-if="idx > 0">{{ " " }}</CodeElement>
      <CodeElement>{{ "[" }}</CodeElement>
      <template v-if="typeof pair.lhs === 'string'">{{ pair.lhs }}</template>
      <ValueRenderer v-else :value="pair.lhs" />
      <CodeElement>{{ " " }}</CodeElement>
      <ValueRenderer :value="pair.rhs" />
      <CodeElement>{{ "]" }}</CodeElement>
    </template>
    <CodeElement v-if="bindings.parenthesizePairs">{{ ")" }}</CodeElement>
    <template v-if="bindings.body">
      <CodeElement>{{ " " }}</CodeElement>
      <ValueRenderer :value="bindings.body" />
    </template>
    <CodeElement>{{ ")" }}</CodeElement>
  </div>
</template>

<style scoped></style>
