<script setup lang="ts">
import type { FunctionDoc } from '../../scheme/docstring/docstring'
import { functionDocSignature } from '../../scheme/docstring/render'
import DocText from './DocText.vue'

defineProps<{ doc: FunctionDoc; id: string }>()
</script>

<template>
  <div :id="id" class="entry">
    <pre class="sig"><code>{{ functionDocSignature(doc) }}</code></pre>
    <p class="desc"><DocText :text="doc.description" /></p>
  </div>
</template>

<style scoped>
.entry {
  border: 1px solid var(--border);
  padding: 1em;
  margin: 1em;
  font-family: var(--font-sans);
}

.sig {
  margin: 1em 0;
  font-family: var(--font-mono);
  font-size: 1em;
  white-space: pre-wrap;
}

.sig code {
  font-family: inherit;
}

.desc {
  margin: 1em 0;
}
</style>
