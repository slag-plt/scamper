<script setup lang="ts">
import { onMounted, shallowRef } from 'vue'
import OutputPane from './OutputPane.vue'
import type { OutputPaneType } from '../composables/use-output-pane'
import hljs from 'highlight.js'
import Scamper from '../../../scamper.js'

const props = defineProps<{
  src: string
  sourceOnly: boolean
}>()

const outputPaneRef = shallowRef<OutputPaneType | null>(null)

function highlightedSource(text: string): string {
  return hljs.highlight(text, { language: 'scheme', ignoreIllegals: true })
    .value
}

onMounted(async () => {
  if (props.sourceOnly) return

  const display = outputPaneRef.value?.display
  if (!display) return

  await Scamper.getInstance().execute({
    src: props.src,
    out: display,
    err: display,
  })
})
</script>

<template>
  <pre
    v-if="sourceOnly"
    class="hljs"
    tabindex="0"
    v-html="highlightedSource(src)"
  />
  <div v-else class="scamper-output">
    <OutputPane ref="outputPaneRef" />
  </div>
</template>

<style scoped>
.scamper-output {
  height: 100%;
}
</style>
