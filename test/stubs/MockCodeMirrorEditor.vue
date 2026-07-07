<script setup lang="ts">
import { onMounted, onUnmounted, ref } from "vue"
import { Loc } from "../../src/lpm"
import type { CodeMirrorEditorAdapter } from "../../src/web/components/codemirror-editor-adapter"
import { useEditorRegistration } from "../../src/web/components/editor-context"
import { noLoadedFileText } from "../../src/web/codemirror/codemirror"
import { mockEditorHandle } from "./mock-editor-handle"

const emit = defineEmits<{ dirty: [] }>()
const editorRegistration = useEditorRegistration()
const src = ref("")
const loaded = ref(false)

const adapter = {
  getDoc() {
    return src.value
  },
  isLoaded() {
    return loaded.value
  },
  initializeDoc(nextSrc: string) {
    loaded.value = true
    src.value = nextSrc
  },
  initializeDummyDoc() {
    loaded.value = false
    src.value = noLoadedFileText
  },
  getCursorLoc() {
    return new Loc(0, 0, 0)
  },
} satisfies CodeMirrorEditorAdapter

onMounted(() => {
  editorRegistration.register(adapter)
  mockEditorHandle.adapter = adapter
})

onUnmounted(() => {
  editorRegistration.unregister(adapter)
  if (mockEditorHandle.adapter === adapter) {
    mockEditorHandle.adapter = null
  }
})

function onInput(event: Event): void {
  src.value = (event.target as HTMLTextAreaElement).value
  emit("dirty")
}
</script>

<template>
  <textarea aria-label="Source code" :value="src" @input="onInput" />
</template>
