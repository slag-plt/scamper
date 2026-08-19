<script setup lang="ts">
import { onMounted, onUnmounted, ref, watch } from 'vue'
import { EditorView } from '@codemirror/view'
import {
  editorThemeCompartment,
  editorThemeExtension,
  fontSizeCompartment,
  fontSizeExtension,
  mkNoFileEditorState,
  wordWrapCompartment,
  wordWrapExtension,
} from '../codemirror/codemirror'
import { currentTheme } from '../../../theme'
import { editorFontSize, editorWordWrap } from '../editor-prefs'
import {
  type CodeMirrorEditorAdapter,
  createCodeMirrorEditorAdapter,
} from '../composables/codemirror-editor-adapter'
import { useEditorRegistration } from '../composables/editor-context'
import type { CursorStatus } from '../codemirror/enclosing-form'
import { editShortcut } from '../edit-commands'
import type { MenuItem } from '../menu'
import PopupMenu from './PopupMenu.vue'

const emit = defineEmits<{ dirty: []; cursorChange: [status: CursorStatus] }>()

const editorRegistration = useEditorRegistration()
const containerRef = ref<HTMLDivElement | null>(null)
let editorView: EditorView | null = null
let adapter: CodeMirrorEditorAdapter | null = null

// Right-click context menu. Its commands come off the adapter, which is also
// what the menu bar drives, so the two menus can't disagree about what "Format
// file" does.
const menuOpen = ref(false)
const menuPos = ref({ x: 0, y: 0 })
const menuItems = ref<MenuItem[]>([])

function onContextMenu(e: MouseEvent) {
  const view = editorView
  if (view === null || adapter === null) {
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
  // Read the status after that dispatch, so `onIdentifier` describes where the
  // cursor now is rather than where it was.
  const ed = adapter
  const { readOnly, hasSelection, onIdentifier } = ed.status()

  menuItems.value = [
    { label: 'Go to definition', kbd: editShortcut.goToDefinition, disabled: !onIdentifier, run: () => { ed.goToDefinition() } },
    { label: 'Find references', kbd: editShortcut.findReferences, disabled: !onIdentifier, run: () => { ed.findReferences() } },
    { separator: true },
    { label: 'Format file', kbd: editShortcut.format, disabled: readOnly, run: () => { ed.format() } },
    { separator: true },
    { label: 'Cut', kbd: editShortcut.cut, disabled: readOnly || !hasSelection, run: () => ed.cut() },
    { label: 'Copy', kbd: editShortcut.copy, disabled: !hasSelection, run: () => { ed.copy() } },
    { label: 'Paste', kbd: editShortcut.paste, disabled: readOnly, run: () => { ed.paste() } },
    { label: 'Select all', kbd: editShortcut.selectAll, run: () => { ed.selectAll() } },
  ]
  menuPos.value = { x: e.clientX, y: e.clientY }
  menuOpen.value = true
}

function closeMenu() {
  menuOpen.value = false
  editorView?.focus()
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

// Likewise for the View menu's display preferences, on the document already
// open; a document opened later picks them up when its state is built.
watch(editorFontSize, (px) => {
  editorView?.dispatch({
    effects: fontSizeCompartment.reconfigure(fontSizeExtension(px)),
  })
})

watch(editorWordWrap, (on) => {
  editorView?.dispatch({
    effects: wordWrapCompartment.reconfigure(wordWrapExtension(on)),
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
  <PopupMenu
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
