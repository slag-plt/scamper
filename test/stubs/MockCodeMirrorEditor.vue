<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue'
import { useEditorRegistration } from '../../src/app/web/composables/editor-context'
import { noLoadedFileText } from '../../src/app/web/codemirror/codemirror'
import { makeMockCodeMirrorEditorAdapter } from './mock-code-mirror-editor-adapter'
import { mockEditorHandle } from './mock-editor-handle'

const emit = defineEmits<{ dirty: [] }>()
const editorRegistration = useEditorRegistration()
const src = ref('')
const loaded = ref(false)

const adapter = makeMockCodeMirrorEditorAdapter({
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
})

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
  emit('dirty')
}
</script>

<template>
  <textarea aria-label="Source code" :value="src" @input="onInput" />
</template>
