<script setup lang="ts">
import { computed, inject } from 'vue'
import { Highlight, Layout } from '../ast'
import { ChangedPathKey } from './changed-path'
import CodeElement from '../../lpm/renderers/vue/components/CodeElement.vue'
import ValueRenderer from '../../lpm/renderers/vue/ValueRenderer.vue'

const props = withDefaults(
  defineProps<{
    layout: Layout
    /** This node's position in the layout, as child indices from the root. */
    path?: number[]
    /**
     * Set on the re-entrant render inside the highlight wrapper, so the node
     * draws its contents instead of wrapping itself a second time.
     */
    inHighlight?: boolean
  }>(),
  { path: () => [], inHighlight: false },
)

// Null everywhere but a trace window; there is nothing to compare against.
const changedPath = inject(ChangedPathKey, null)

/** Whether this is the sub-expression that changed since the previous step. */
const isChanged = computed(() => {
  if (props.inHighlight) return false
  const target = changedPath?.value
  return (
    target?.length === props.path.length &&
    target.every((i, at) => i === props.path[at])
  )
})

const DELIMS = {
  paren: ['(', ')'],
  bracket: ['[', ']'],
  brace: ['{', '}'],
} as const

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
  <!-- The changed node wraps itself once, then renders its own contents on the
       way back in. Only one node in the tree ever matches, so nothing else in
       the app gains a wrapper it did not have before. -->
  <span v-if="isChanged" class="trace-changed"
    ><LayoutRenderer :layout="layout" :path="path" in-highlight
  /></span>
  <CodeElement
    v-else-if="layout.kind === 'tok' && layout.hl"
    :class="HL_CLASS[layout.hl]"
    >{{ layout.text }}</CodeElement
  >
  <CodeElement v-else-if="layout.kind === 'tok'">{{ layout.text }}</CodeElement>
  <template v-else-if="layout.kind === 'val'">
    <span v-if="valClass(layout.value)" :class="valClass(layout.value)"
      ><ValueRenderer :value="layout.value"
    /></span>
    <ValueRenderer v-else :value="layout.value" />
  </template>
  <template v-else-if="layout.kind === 'hash'">
    <CodeElement>#</CodeElement>
    <LayoutRenderer :layout="layout.child" :path="[...path, 0]" />
  </template>
  <template v-else>
    <CodeElement>{{ DELIMS[layout.delim][0] }}</CodeElement>
    <template v-for="(child, idx) in layout.children" :key="idx">
      <CodeElement v-if="idx > 0">{{ " " }}</CodeElement>
      <LayoutRenderer :layout="child" :path="[...path, idx]" />
    </template>
    <CodeElement>{{ DELIMS[layout.delim][1] }}</CodeElement>
  </template>
</template>

<style scoped>
/* The sub-expression that moved since the previous step. Tinted rather than
   coloured: the tokens inside keep their own syntax highlighting. */
.trace-changed {
  background: color-mix(in srgb, var(--accent) 25%, transparent);
  border-radius: 3px;
  padding: 0 0.1em;
}
</style>
