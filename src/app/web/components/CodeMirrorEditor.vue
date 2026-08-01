<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from 'vue'
import { EditorView } from '@codemirror/view'
import { findReferences, jumpToDefinition } from '@codemirror/lsp-client'
import {
  editorThemeCompartment,
  editorThemeExtension,
  mkNoFileEditorState,
} from '../codemirror/codemirror'
import { formatScamperDocument } from '../codemirror/extensions/prettier'
import { currentTheme } from '../../../theme'
import {
  type CodeMirrorEditorAdapter,
  createCodeMirrorEditorAdapter,
} from '../composables/codemirror-editor-adapter'
import { useEditorRegistration } from '../composables/editor-context'
import type { CursorStatus } from '../codemirror/enclosing-form'
import { identifierAt } from '../../../scheme/token'
import EditorContextMenu, { type MenuItem } from './EditorContextMenu.vue'

const emit = defineEmits<{ dirty: []; cursorChange: [status: CursorStatus] }>()

const editorRegistration = useEditorRegistration()
const containerRef = ref<HTMLDivElement | null>(null)
let editorView: EditorView | null = null
let adapter: CodeMirrorEditorAdapter | null = null

// Right-click context menu.
const isMac = /Mac|iPhone|iPad|iPod/i.test(
  typeof navigator === 'undefined' ? '' : navigator.userAgent,
)
const mod = isMac ? 'Cmd' : 'Ctrl'
const menuOpen = ref(false)
const menuPos = ref({ x: 0, y: 0 })
const menuItems = ref<MenuItem[]>([])

function onContextMenu(e: MouseEvent) {
  const view = editorView
  if (view === null) {
    return
  }
  const pos = view.posAtCoords({ x: e.clientX, y: e.clientY })
  if (pos === null) {
    return // not over the content -- leave the native menu for gutters/margins
  }
  e.preventDefault()

  // Right-clicking outside a selection moves the cursor there, so the actions
  // operate on the clicked location; a click inside a selection keeps it.
  const sel = view.state.selection.main
  const inSelection = !sel.empty && pos >= sel.from && pos <= sel.to
  if (!inSelection) {
    view.dispatch({ selection: { anchor: pos } })
  }
  const targetPos = inSelection ? sel.head : pos
  const onIdentifier =
    identifierAt(view.state.doc.toString(), targetPos) !== undefined
  const hasSelection = !view.state.selection.main.empty
  const readOnly = view.state.readOnly

  menuItems.value = [
    { label: 'Go to definition', kbd: 'Alt+.', disabled: !onIdentifier, run: () => { jumpToDefinition(view) } },
    { label: 'Find references', kbd: 'Shift+Alt+.', disabled: !onIdentifier, run: () => { findReferences(view) } },
    { separator: true },
    { label: 'Format file', kbd: `${mod}+Shift+I`, disabled: readOnly, run: () => { formatScamperDocument(view) } },
    { separator: true },
    { label: 'Cut', disabled: readOnly || !hasSelection, run: () => { cutSelection(view) } },
    { label: 'Copy', disabled: !hasSelection, run: () => { copySelection(view) } },
    { label: 'Paste', disabled: readOnly, run: () => { pasteClipboard(view) } },
    { label: 'Select all', run: () => { selectAllText(view) } },
  ]
  menuPos.value = { x: e.clientX, y: e.clientY }
  menuOpen.value = true
}

function closeMenu() {
  menuOpen.value = false
  editorView?.focus()
}

function copySelection(view: EditorView) {
  const { from, to } = view.state.selection.main
  void navigator.clipboard.writeText(view.state.sliceDoc(from, to))
}

function cutSelection(view: EditorView) {
  const { from, to } = view.state.selection.main
  void navigator.clipboard.writeText(view.state.sliceDoc(from, to))
  view.dispatch({ changes: { from, to } })
}

function pasteClipboard(view: EditorView) {
  void navigator.clipboard
    .readText()
    .then((text) => {
      const { from, to } = view.state.selection.main
      view.dispatch({
        changes: { from, to, insert: text },
        selection: { anchor: from + text.length },
      })
    })
    .catch(() => {
      /* clipboard read unavailable or denied */
    })
}

function selectAllText(view: EditorView) {
  view.dispatch({ selection: { anchor: 0, head: view.state.doc.length } })
}

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
  <div
    ref="containerRef"
    class="codemirror-editor"
    @contextmenu="onContextMenu"
  ></div>
  <EditorContextMenu
    v-if="menuOpen"
    :x="menuPos.x"
    :y="menuPos.y"
    :items="menuItems"
    @close="closeMenu"
  />
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
