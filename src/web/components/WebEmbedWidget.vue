<script setup lang="ts">
import { shallowRef, onMounted } from "vue"
import { ScamperVue } from "../../scamper-vue"
import { ScamperError } from "../../lpm/error"
import OutputPane from "./OutputPane.vue"
import type { OutputPaneType } from "./use-output-pane"
import hljs from "highlight.js"

const props = defineProps<{
  src: string
  sourceOnly: boolean
}>()

const outputPaneRef = shallowRef<OutputPaneType | null>(null)

function highlightedSource(text: string): string {
  return hljs.highlight(text, { language: "scheme", ignoreIllegals: true })
    .value
}

onMounted(async () => {
  if (props.sourceOnly) return

  const display = outputPaneRef.value?.display
  if (!display) return

  try {
    await new ScamperVue(display, props.src).runProgram()
  } catch (e) {
    if (e instanceof ScamperError) {
      display.report(e)
    } else if (e instanceof Error) {
      display.report(new ScamperError("Runtime", e.message))
    }
  }
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
