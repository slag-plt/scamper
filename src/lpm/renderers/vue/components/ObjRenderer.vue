<script setup lang="ts">
import { Value } from '../../../index'
import { escapeStringLiteral } from '../../../util'
import CodeElement from './CodeElement.vue'
import ValueRenderer from '../ValueRenderer.vue'

// A map value (what a `{...}` literal builds), rendered as
// `{ "k1" : v1, "k2" : v2 }`. Values go through ValueRenderer so a map holding
// an image, a list, or another map renders those properly.
const props = defineProps<{ value: Record<string, Value> }>()

const entries = () =>
  Object.keys(props.value).map((k) => ({
    key: `"${escapeStringLiteral(k)}"`,
    value: props.value[k],
  }))
</script>

<template>
  <CodeElement v-if="Object.keys(value).length === 0">{{ "{}" }}</CodeElement>
  <template v-else>
    <CodeElement>{{ "{ " }}</CodeElement>
    <template v-for="(entry, index) in entries()" :key="entry.key">
      <CodeElement v-if="index > 0">{{ ", " }}</CodeElement>
      <CodeElement>{{ entry.key }}{{ " : " }}</CodeElement>
      <ValueRenderer :value="entry.value" />
    </template>
    <CodeElement>{{ " }" }}</CodeElement>
  </template>
</template>

<style scoped></style>
