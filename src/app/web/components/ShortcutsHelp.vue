<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue'

// A (?) button that reveals a cheat-sheet of the editor's keyboard shortcuts.
const open = ref(false)
const root = ref<HTMLElement | null>(null)

// Modifier names differ by platform (Cmd on macOS, Ctrl elsewhere). CodeMirror
// binds "Mod" to that platform key; a few bindings differ per platform too.
const isMac = /Mac|iPhone|iPad|iPod/i.test(
  typeof navigator === 'undefined' ? '' : navigator.userAgent,
)
const mod = isMac ? 'Cmd' : 'Ctrl'
const foldPrefix = isMac ? ['Cmd', 'Alt'] : ['Ctrl', 'Shift']

interface Shortcut {
  label: string
  keys?: string[]
  note?: string
}
interface Group {
  title: string
  items: Shortcut[]
}

const groups: Group[] = [
  {
    title: 'Code',
    items: [
      { label: 'Autocomplete', keys: ['Ctrl', 'Space'] },
      { label: 'Signature help', keys: [mod, 'Shift', 'Space'], note: 'auto too' },
      { label: 'Go to definition', keys: ['Alt', '.'] },
      { label: 'Find references', keys: ['Shift', 'Alt', '.'] },
      { label: 'Documentation', note: 'hover a name' },
    ],
  },
  {
    title: 'Edit',
    items: [
      { label: 'Format file', keys: [mod, 'Shift', 'I'] },
      { label: 'Toggle comment', keys: [mod, '/'] },
      { label: 'Indent', keys: ['Tab'] },
      { label: 'Undo', keys: [mod, 'Z'] },
      { label: 'Redo', keys: [mod, 'Shift', 'Z'] },
    ],
  },
  {
    title: 'Find & run',
    items: [
      { label: 'Find in file', keys: [mod, 'F'] },
      { label: 'Fold / unfold', keys: foldPrefix, note: '[ or ]' },
      { label: 'Run program', note: '▶ button · access key W' },
    ],
  },
]

function onDocMouseDown(e: MouseEvent) {
  if (open.value && root.value && !root.value.contains(e.target as Node)) {
    open.value = false
  }
}

function onKeyDown(e: KeyboardEvent) {
  if (open.value && e.key === 'Escape') {
    open.value = false
  }
}

onMounted(() => {
  document.addEventListener('mousedown', onDocMouseDown)
  document.addEventListener('keydown', onKeyDown)
})
onUnmounted(() => {
  document.removeEventListener('mousedown', onDocMouseDown)
  document.removeEventListener('keydown', onKeyDown)
})
</script>

<template>
  <span ref="root" class="shortcuts-help">
    <button
      class="fa-solid fa-circle-question"
      aria-label="Keyboard shortcuts"
      aria-haspopup="dialog"
      :aria-expanded="open"
      @click="open = !open"
    ></button>
    <div v-if="open" class="panel" role="dialog" aria-label="Keyboard shortcuts">
      <div class="panel-title">Keyboard shortcuts</div>
      <div v-for="group in groups" :key="group.title" class="group">
        <div class="group-title">{{ group.title }}</div>
        <div v-for="item in group.items" :key="item.label" class="row">
          <span class="label">{{ item.label }}</span>
          <span class="keys">
            <template v-for="(k, i) in item.keys ?? []" :key="i">
              <kbd>{{ k }}</kbd
              ><span v-if="i < (item.keys?.length ?? 0) - 1" class="plus">+</span>
            </template>
            <span v-if="item.note" class="note">{{ item.note }}</span>
          </span>
        </div>
      </div>
    </div>
  </span>
</template>

<style scoped>
.shortcuts-help {
  position: relative;
  display: inline-flex;
}

.panel {
  position: absolute;
  top: calc(100% + 6px);
  right: 0;
  z-index: 10;
  min-width: 250px;
  max-width: 320px;
  padding: 0.6em 0.75em 0.75em;
  background: var(--surface);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: 6px;
  box-shadow: 0 6px 20px rgba(0, 0, 0, 0.25);
  font-size: 0.85rem;
  text-align: left;
  cursor: default;
}

.panel-title {
  font-weight: 600;
  margin-bottom: 0.25em;
}

.group-title {
  margin: 0.55em 0 0.15em;
  font-size: 0.68rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  opacity: 0.55;
}

.row {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  gap: 1em;
  padding: 0.12em 0;
}

.label {
  white-space: nowrap;
}

.keys {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: flex-end;
  gap: 0.2em;
}

kbd {
  padding: 0.05em 0.4em;
  font-family: inherit;
  font-size: 0.75rem;
  background: var(--surface-muted);
  border: 1px solid var(--border);
  border-radius: 4px;
  white-space: nowrap;
}

.plus {
  font-size: 0.7rem;
  opacity: 0.5;
}

.note {
  font-size: 0.75rem;
  font-style: italic;
  opacity: 0.6;
}
</style>
