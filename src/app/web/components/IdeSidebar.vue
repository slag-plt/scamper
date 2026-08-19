<script setup lang="ts">
import { computed, ref } from 'vue'
import type { FileEntry } from '../../../fs/fs'
import PopupMenu from './PopupMenu.vue'
import type { MenuItem } from '../menu'

const props = defineProps<{
  files?: FileEntry[]
  currentFile?: string | null
  /** Whether this deployment has a file server at all; hides the account block. */
  hasServer?: boolean
  /** Whether that server offers a way to sign in (false for the dev stub). */
  canSignIn?: boolean
  /** Whose files are being edited, or null when they are this browser's own. */
  signedInAs?: string | null
  /** Whether the server is answering. */
  connection?: 'online' | 'offline'
  signIn?: () => void
  signOut?: () => void
  create?: () => void
  /** Asks for a file to upload; the app owns the picker (see IdeApp). */
  upload?: () => void
  archive?: () => void
  /** The file operations, each naming the file it acts on. */
  rename?: (filename: string) => void
  duplicate?: (filename: string) => void
  deleteFile?: (filename: string) => void
  download?: (filename: string) => void
  /** Opens the history browser, on `filename` if one is named. */
  history?: (filename?: string) => void
  selectFile?: (filename: string) => void
  fileDrop?: (files: FileList) => Promise<void>
}>()

const isDragOver = ref(false)

// The file whose menu is open, and where to draw it.
const menuFile = ref<string | null>(null)
const menuPos = ref({ x: 0, y: 0 })

const menuItems = computed<MenuItem[]>(() => {
  const filename = menuFile.value
  if (filename === null) return []
  return [
    { label: 'Rename…', run: () => props.rename?.(filename) },
    { label: 'Duplicate…', run: () => props.duplicate?.(filename) },
    { label: 'Download', run: () => props.download?.(filename) },
    { label: 'History…', run: () => props.history?.(filename) },
    { separator: true },
    { label: 'Delete', danger: true, run: () => props.deleteFile?.(filename) },
  ]
})

/**
 * Toggles the menu below the ⋯ button, so it points at the file it acts on
 * rather than at wherever the pointer happened to be. The button swallows its
 * own mousedown, which is what the menu closes on -- otherwise a second click
 * on the same button would close and immediately reopen it.
 */
function toggleFileMenu(filename: string, event: MouseEvent) {
  if (menuFile.value === filename) {
    menuFile.value = null
    return
  }
  const rect = (event.currentTarget as HTMLElement).getBoundingClientRect()
  menuPos.value = { x: rect.left, y: rect.bottom + 2 }
  menuFile.value = filename
}

function handleDragOver(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    event.dataTransfer.dropEffect = 'copy'
  }
  isDragOver.value = true
}

function handleDragLeave(event: DragEvent) {
  event.preventDefault()
  isDragOver.value = false
}

async function handleDrop(event: DragEvent) {
  event.preventDefault()
  isDragOver.value = false
  const droppedFiles = event.dataTransfer?.files
  if (droppedFiles && droppedFiles.length > 0) {
    await props.fileDrop?.(droppedFiles)
  }
}
</script>

<template>
  <div
    class="ide-sidebar"
    :class="{ 'drag-over': isDragOver }"
    @dragover="handleDragOver"
    @dragleave="handleDragLeave"
    @drop="handleDrop"
  >
    <!-- Above the file buttons rather than in the status bar: the server is the
         difference between files that survive this browser and files that do
         not, which is worth seeing before you start rather than after. -->
    <div v-if="hasServer" class="sidebar-account">
      <div class="account-line">
        <span
          class="status-dot"
          :class="connection ?? 'online'"
          aria-hidden="true"
        ></span>
        <span class="account-who" :title="signedInAs ?? undefined">
          {{
            canSignIn ? (signedInAs ?? 'Not signed in') : 'Development server'
          }}
        </span>
      </div>
      <button
        v-if="canSignIn && signedInAs"
        type="button"
        class="account-action"
        @click="signOut?.()"
      >
        Sign out
      </button>
      <button
        v-else-if="canSignIn"
        type="button"
        class="account-action"
        @click="signIn?.()"
      >
        Sign in to save your files
      </button>
      <p v-if="connection === 'offline'" class="account-offline" role="status">
        {{
          signedInAs
            ? 'Offline — your changes are not being saved.'
            : 'Offline — the server cannot be reached.'
        }}
      </p>
    </div>
    <!-- Only the operations that are about the drawer as a whole. Anything
         about one file lives in that file's own menu, below. -->
    <div class="sidebar-actions">
      <button
        type="button"
        class="icon-button fa-solid fa-file"
        title="Create file"
        aria-label="Create file"
        @click="create?.()"
      ></button>
      <button
        type="button"
        class="icon-button fa-solid fa-upload"
        title="Upload file"
        aria-label="Upload file"
        @click="upload?.()"
      ></button>
      <button
        type="button"
        class="icon-button fa-solid fa-file-zipper"
        title="Download all files as a zip archive"
        aria-label="Download all files as a zip archive"
        :disabled="!props.files?.length"
        @click="archive?.()"
      ></button>
      <!-- Stays here as well as in each file's menu: a deleted file has no row
           to open a menu on, and recovering one is what this is for. -->
      <button
        type="button"
        class="icon-button fa-solid fa-clock-rotate-left"
        title="File history"
        aria-label="File history"
        @click="history?.()"
      ></button>
    </div>
    <div class="file-drawer">
      <div
        v-for="file in props.files"
        :key="file.name"
        class="file"
        :class="{ selected: file.name === props.currentFile }"
      >
        <!-- The name and the ⋯ are each their own button rather than one
             clickable row holding another button, which no screen reader
             makes sense of. -->
        <button
          type="button"
          class="file-name"
          :title="file.name"
          :aria-label="`Open ${file.name}`"
          @click="selectFile?.(file.name)"
        >
          {{ file.name }}
        </button>
        <button
          class="file-menu-button fa-solid fa-ellipsis-vertical"
          :class="{ open: menuFile === file.name }"
          type="button"
          :title="`Actions for ${file.name}`"
          :aria-label="`Actions for ${file.name}`"
          aria-haspopup="menu"
          :aria-expanded="menuFile === file.name"
          @mousedown.stop
          @click.stop="toggleFileMenu(file.name, $event)"
        ></button>
      </div>
    </div>
    <PopupMenu
      v-if="menuFile !== null"
      :x="menuPos.x"
      :y="menuPos.y"
      :items="menuItems"
      @close="menuFile = null"
    />
  </div>
</template>

<style scoped>
.ide-sidebar {
  display: flex;
  flex-direction: column;
  flex: 1;
  height: 100%;
  background: var(--surface-sidebar);
}

.ide-sidebar.drag-over {
  opacity: 0.5;
  background: var(--surface-hover);
  transition:
    opacity 0.2s ease,
    background 0.2s ease;
}

.sidebar-account {
  padding: var(--space-sm) var(--space-md);
  border-bottom: 1px solid var(--border);
  font-size: var(--text-sm);
  text-align: center;
}

.account-line {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.4em;
  /* The address can be long and the drawer is narrow; truncate rather than
     stretch the sidebar or wrap into two lines. */
  min-width: 0;
}

.account-who {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.status-dot {
  flex-shrink: 0;
  width: 0.55em;
  height: 0.55em;
  border-radius: 50%;
  background: var(--accent);
}

.status-dot.offline {
  background: var(--danger);
}

.account-action {
  border-radius: var(--radius-sm);
  margin-top: 0.15em;
  border: none;
  background: none;
  padding: 0;
  font: inherit;
  color: var(--link);
  text-decoration: underline;
  cursor: pointer;
}

.account-offline {
  margin: 0.25em 0 0;
  font-size: 0.9em;
  opacity: 0.8;
}

.sidebar-actions {
  padding: var(--space-xs) var(--space-md);
  display: flex;
  align-items: center;
  gap: var(--space-2xs);
  border-bottom: 1px solid var(--border);
}

.file-drawer {
  display: flex;
  flex-direction: column;
  gap: 1px;
  padding: var(--space-xs);
  flex: 1;
  overflow-x: hidden;
  /* `auto`, not `scroll`: a permanent gutter is a real cost in a narrow
     column that is usually short enough not to need one. */
  overflow-y: auto;
}

.file {
  display: flex;
  align-items: center;
  border-radius: var(--radius-md);
  /* Reserved so the selected row's rule does not shift the name sideways. */
  box-shadow: inset 2px 0 0 transparent;
}

.file:hover:not(.selected) {
  background: var(--surface-hover);
}

/* A tint and a rule rather than a saturated fill. The full-bleed --accent bar
   made the open file the loudest thing on the screen and read as a banner. */
.file.selected {
  background: var(--selected-bg);
  color: var(--selected-fg);
  box-shadow: inset 2px 0 0 var(--brand);
}

/* Takes the whole row bar the ⋯, so clicking a file still means clicking
   anywhere along it. Long names truncate rather than pushing the ⋯ out. */
.file-name {
  flex: 1;
  min-width: 0;
  border: none;
  background: none;
  padding: var(--space-xs) var(--space-md);
  font: inherit;
  font-size: var(--text-md);
  color: inherit;
  text-align: left;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  cursor: pointer;
  border-radius: var(--radius-md);
}

.file.selected .file-name {
  font-weight: 600;
}

/* Only the open file carries one, the way Overleaf does it: a column of ⋯ down
   every entry is noise, and the operations are about the file you are working
   on. `visibility` rather than `display` keeps the space reserved, so names
   don't shift as the selection moves -- and unlike `opacity` it also takes the
   hidden buttons out of the tab order and off the accessibility tree, which is
   what a control that isn't really there should do. */
.file-menu-button {
  flex-shrink: 0;
  visibility: hidden;
  border: none;
  background: none;
  padding: var(--space-xs) var(--space-sm);
  font-size: var(--text-md);
  line-height: 1;
  color: inherit;
  cursor: pointer;
  border-radius: var(--radius-sm);
}

.file.selected .file-menu-button {
  visibility: visible;
}

/* Tinted with the row's own text colour, since the row it sits on is the
   selected one rather than the plain sidebar. */
.file-menu-button:hover,
.file-menu-button.open {
  background: color-mix(in srgb, currentColor 18%, transparent);
}
</style>
