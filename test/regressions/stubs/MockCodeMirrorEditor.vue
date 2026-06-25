<script setup lang="ts">
import { onMounted, onUnmounted, ref } from "vue"
import { Loc } from "../../../src/lpm"
import type { CodeMirrorEditorAdapter } from "../../../src/web/components/codemirror-editor-adapter"
import { useEditorRegistration } from "../../../src/web/components/editor-context"

const emit = defineEmits<{ dirty: [] }>()
const editorRegistration = useEditorRegistration()
const src = ref("")

const adapter = {
  getDoc() {
    return src.value
  },
  initializeDoc(nextSrc: string) {
    src.value = nextSrc
  },
  initializeDummyDoc() {
    src.value = ""
  },
  getCursorLoc() {
    return new Loc(0, 0, 0)
  },
} satisfies CodeMirrorEditorAdapter

onMounted(() => {
  editorRegistration.register(adapter)
})

onUnmounted(() => {
  editorRegistration.unregister(adapter)
})

function onInput(event: Event): void {
  src.value = (event.target as HTMLTextAreaElement).value
  emit("dirty")
}
</script>

<template>
  <textarea aria-label="Source code" :value="src" @input="onInput" />
</template>
