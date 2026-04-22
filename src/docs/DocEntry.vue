<script setup lang="ts">
import { computed } from "vue"
import { Doc } from "./api/docs.js"

const props = defineProps<{ doc: Doc; id: string }>()

function signature(doc: Doc): string {
  if (doc.args.length === 0) {
    return `${doc.name} : ${doc.returnType}`
  }
  const argNames = doc.args.map((a) => a.name).join(" ")
  const argLines = doc.args.map((a) => `  ${a.name}: ${a.desc}`).join("\n")
  return `(${doc.name} ${argNames}) -> ${doc.returnType}\n${argLines}`
}

interface TextSpan {
  text: string
  code: boolean
}

const descSpans = computed<TextSpan[]>(() => {
  const parts = props.doc.desc.trim().split("`")
  return parts.map((text, i) => ({ text, code: i % 2 === 1 }))
})
</script>

<template>
  <div :id="id" class="entry">
    <pre class="sig"><code>{{ signature(doc) }}</code></pre>
    <p class="desc">
      <template v-for="(span, i) in descSpans" :key="i">
        <code v-if="span.code">{{ span.text }}</code>
        <template v-else>{{ span.text }}</template>
      </template>
    </p>
  </div>
</template>

<style scoped>
.entry {
  border: 1px solid black;
  padding: 1em;
  margin: 1em;
  font-family: -apple-system, BlinkMacSystemFont, avenir next, avenir, segoe ui,
    helvetica neue, helvetica, Cantarell, Ubuntu, roboto, noto, arial,
    sans-serif;
}

.sig {
  margin: 1em 0;
  font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
  font-size: 1em;
  white-space: pre-wrap;
}

.sig code {
  font-family: inherit;
}

.desc {
  margin: 1em 0;
}

.desc code {
  font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
}
</style>
