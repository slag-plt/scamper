<script setup lang="ts">
import { Highlight, Layout } from '../ast'
import CodeElement from '../../lpm/renderers/vue/components/CodeElement.vue'
import ValueRenderer from '../../lpm/renderers/vue/ValueRenderer.vue'

defineProps<{ layout: Layout }>()

const DELIMS = { paren: ['(', ')'], bracket: ['[', ']'] } as const

// Syntax highlighting is done by tagging the HTML directly: because we build the
// output from the AST, each token's role is already known, so there is no need
// to run the hljs tokenizer -- we just attach the scamper-hl-* classes that
// public/css/scamper-highlight.css themes (light/dark). Keywords are tagged on
// the token (Highlight); literal values are classed by their runtime type.
const HL_CLASS: Record<Highlight, string> = {
  keyword: 'scamper-hl-keyword',
}

/** Highlight class for a literal value leaf, by runtime type ('' = default). */
function valClass(v: unknown): string {
  if (typeof v === 'number') return 'scamper-hl-number'
  if (typeof v === 'string') return 'scamper-hl-string'
  if (typeof v === 'boolean' || v === null) return 'scamper-hl-literal'
  return ''
}
</script>

<!-- The web backend of a Layout (src/scheme/ast.ts): the DOM counterpart of
     layoutToString. One recursive, all-inline component renders every surface
     form, so let/match/cond sit on the same line as their trace arrow and match
     their text form exactly. `val` leaves defer to ValueRenderer so values
     substituted into a trace (numbers, lists, images, ...) render correctly. -->
<template>
  <CodeElement v-if="layout.kind === 'tok' && layout.hl" :class="HL_CLASS[layout.hl]"
    >{{ layout.text }}</CodeElement
  >
  <CodeElement v-else-if="layout.kind === 'tok'">{{ layout.text }}</CodeElement>
  <template v-else-if="layout.kind === 'val'">
    <span v-if="valClass(layout.value)" :class="valClass(layout.value)"
      ><ValueRenderer :value="layout.value"
    /></span>
    <ValueRenderer v-else :value="layout.value" />
  </template>
  <template v-else>
    <CodeElement>{{ DELIMS[layout.delim][0] }}</CodeElement>
    <template v-for="(child, idx) in layout.children" :key="idx">
      <CodeElement v-if="idx > 0">{{ " " }}</CodeElement>
      <LayoutRenderer :layout="child" />
    </template>
    <CodeElement>{{ DELIMS[layout.delim][1] }}</CodeElement>
  </template>
</template>

<style scoped></style>
