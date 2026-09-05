<script setup lang="ts">
import { computed } from 'vue'

/**
 * Documentation prose, with `backticked` spans set as code.
 *
 * The whole of the markup a docstring gets. Shared by a function's description
 * and a module's blurb (#411) so the two read as one thing rather than two
 * renderings that drifted apart.
 */
const props = defineProps<{ text: string }>()

interface TextSpan {
  text: string
  code: boolean
}

// Splitting on the backtick makes every odd-numbered piece the code one.
const spans = computed<TextSpan[]>(() =>
  props.text
    .trim()
    .split('`')
    .map((text, i) => ({ text, code: i % 2 === 1 })),
)
</script>

<template>
  <template v-for="(span, i) in spans" :key="i">
    <code v-if="span.code">{{ span.text }}</code>
    <template v-else>{{ span.text }}</template>
  </template>
</template>

<style scoped>
/* Here rather than in whatever renders this: the root below is a v-for
   fragment, so Vue never passes a parent's scope id down to these elements and
   a `code` rule written in the parent would match nothing. */
code {
  font-family: var(--font-mono);
}
</style>
