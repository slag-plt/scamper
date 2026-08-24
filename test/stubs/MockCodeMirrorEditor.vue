<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue'
import { useEditorRegistration } from '../../src/app/web/composables/editor-context'
import { noLoadedFileText } from '../../src/app/web/codemirror/codemirror'
import { makeMockCodeMirrorEditorAdapter } from './mock-code-mirror-editor-adapter'
import { mockEditorHandle } from './mock-editor-handle'

const emit = defineEmits<{
  dirty: []
  cursorChange: [status: { line: number; column: number; path: string[] }]
}>()

/** Reports where the cursor is, as the real editor does on every load and move. */
function reportCursor() {
  emit('cursorChange', {
    line: 1,
    column: 1,
    path: [...mockEditorHandle.cursorPath],
  })
}

const editorRegistration = useEditorRegistration()
const src = ref('')
const loaded = ref(false)
const readOnly = ref(false)

const adapter = makeMockCodeMirrorEditorAdapter(
  {
    getDoc() {
      return src.value
    },
    isLoaded() {
      return loaded.value
    },
    initializeDoc(nextSrc: string, isReadOnly = false) {
      loaded.value = true
      src.value = nextSrc
      readOnly.value = isReadOnly
      reportCursor()
    },
    initializeDummyDoc() {
      loaded.value = false
      src.value = noLoadedFileText
    },
    replaceDoc(nextSrc: string) {
      // The real adapter applies this as an undoable edit; for the stub the
      // observable part is that the document changed and the file is dirty.
      src.value = nextSrc
      emit('dirty')
    },
    status: () => ({
      // The real editor's no-file state is read-only, and the menus grey
      // themselves out from this, so the stub keeps that tie. An internal file
      // opened for looking at is read-only too (#178).
      readOnly: !loaded.value || readOnly.value,
      hasSelection: false,
      canUndo: false,
      canRedo: false,
      onIdentifier: false,
    }),
  },
  { calls: mockEditorHandle.commands },
)

onMounted(() => {
  editorRegistration.register(adapter)
  mockEditorHandle.adapter = adapter
  mockEditorHandle.commands.length = 0
  reportCursor()
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
