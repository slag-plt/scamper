<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from 'vue'
import { EditorView } from '@codemirror/view'
import {
  editorThemeCompartment,
  editorThemeExtension,
  mkNoFileEditorState,
} from '../codemirror/codemirror'
import { currentTheme } from '../../../theme'
import {
  type CodeMirrorEditorAdapter,
  createCodeMirrorEditorAdapter,
} from '../composables/codemirror-editor-adapter'
import { useEditorRegistration } from '../composables/editor-context'
import type { CursorStatus } from '../codemirror/enclosing-form'

const emit = defineEmits<{ dirty: []; cursorChange: [status: CursorStatus] }>()

const editorRegistration = useEditorRegistration()
const containerRef = ref<HTMLDivElement | null>(null)
let editorView: EditorView | null = null
let adapter: CodeMirrorEditorAdapter | null = null

onMounted(() => {
  if (!containerRef.value) return
  const emitCursorChange = (status: CursorStatus) => {
    emit('cursorChange', status)
  }
  editorView = new EditorView({
    state: mkNoFileEditorState(emitCursorChange),
    parent: containerRef.value,
  })
  adapter = createCodeMirrorEditorAdapter(
    editorView,
    () => {
      emit('dirty')
    },
    emitCursorChange,
  )
  editorRegistration.register(adapter)
})

// Live-swap the editor theme when the app theme changes.
watch(currentTheme, (theme) => {
  editorView?.dispatch({
    effects: editorThemeCompartment.reconfigure(editorThemeExtension(theme)),
  })
})

onUnmounted(() => {
  if (adapter) {
    adapter.destroy()
    editorRegistration.unregister(adapter)
    adapter = null
  }
  editorView?.destroy()
  editorView = null
})
</script>

<template>
  <div ref="containerRef" class="codemirror-editor"></div>
</template>

<style scoped>
.codemirror-editor {
  font-family:
    Menlo, Consolas, Monaco, "Liberation Mono", "Lucida Console", monospace;
  font-size: 1em;
  height: 100%;
  overflow: hidden;
}

:deep(.cm-editor) {
  height: 100%;
  max-height: 100%;
}

:deep(.cm-scroller) {
  overflow: auto;
}

:deep(.cm-editor .cm-content) {
  font-family:
    Menlo, Consolas, Monaco, "Liberation Mono", "Lucida Console", monospace;
  font-size: 1em;
}
</style>
