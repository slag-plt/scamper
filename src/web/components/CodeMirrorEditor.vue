<script setup lang="ts">
import { onMounted, onUnmounted, ref } from "vue"
import { EditorView } from "@codemirror/view"
import { mkFreshEditorState, mkNoFileEditorState } from "../codemirror"
import { CodeMirrorEditorType } from "./use-codemirror-editor"
import { Loc } from "../../lpm"

const emit = defineEmits<{ dirty: [] }>()

const containerRef = ref<HTMLDivElement | null>(null)
let editorView: EditorView | null = null

function getDoc(): string {
  return editorView?.state.doc.toString() ?? ""
}

function initializeDoc(src: string): void {
  editorView?.setState(
    mkFreshEditorState(src, {
      dirtyAction: () => {
        emit("dirty")
      },
      isReadOnly: false,
    }),
  )
}

function initializeDummyDoc(): void {
  editorView?.setState(mkNoFileEditorState())
}

function getCursorLoc(): Loc | null {
  if (!editorView) {
    return null
  }
  const idx = editorView.state.selection.main.head
  const line = editorView.state.doc.lineAt(idx)
  return new Loc(line.number, idx - line.from, idx)
}

onMounted(() => {
  if (!containerRef.value) return
  editorView = new EditorView({
    state: mkNoFileEditorState(),
    parent: containerRef.value,
  })
})

onUnmounted(() => {
  editorView?.destroy()
  editorView = null
})

defineExpose<CodeMirrorEditorType>({
  getDoc,
  initializeDoc,
  initializeDummyDoc,
  getCursorLoc,
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
