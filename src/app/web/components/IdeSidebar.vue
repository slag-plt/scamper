<script setup lang="ts">
import { ref } from 'vue'
import type { FileEntry } from '../../../fs/fs'

const props = defineProps<{
  version?: string
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
  rename?: () => void
  deleteFile?: () => void
  download?: () => void
  archive?: () => void
  history?: () => void
  selectFile?: (filename: string) => void
  uploadFile?: (file: File) => Promise<void>
  fileDrop?: (files: FileList) => Promise<void>
}>()

const isDragOver = ref(false)
const fileInputRef = ref<HTMLInputElement | null>(null)

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

function handleUploadClick() {
  fileInputRef.value?.click()
}

async function handleFileInputChange(event: Event) {
  const target = event.target as HTMLInputElement
  const file = target.files?.[0]
  if (file) {
    await props.uploadFile?.(file)
  }
  target.value = ''
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
    <div class="sidebar-title">
      Scamper <span v-if="version">{{ version }}</span>
    </div>
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
          {{ canSignIn ? (signedInAs ?? 'Not signed in') : 'Development server' }}
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
    <div class="sidebar-actions">
      <button
        class="fa-solid fa-file"
        aria-label="Create file"
        @click="create?.()"
      ></button>
      <input
        ref="fileInputRef"
        type="file"
        style="display: none"
        @change="handleFileInputChange"
      />
      <button
        class="fa-solid fa-upload"
        aria-label="Upload file"
        @click="handleUploadClick"
      ></button>
      <button
        class="fa-solid fa-file-zipper"
        aria-label="Download all files as a zip archive"
        :disabled="!props.files?.length"
        @click="archive?.()"
      ></button>
      ⋅
      <button
        class="fa-solid fa-pencil"
        aria-label="Rename file"
        :disabled="!props.currentFile"
        @click="rename?.()"
      ></button>
      <button
        class="fa-solid fa-trash"
        aria-label="Delete file"
        :disabled="!props.currentFile"
        @click="deleteFile?.()"
      ></button>
      <button
        class="fa-solid fa-download"
        aria-label="Download file"
        :disabled="!props.currentFile"
        @click="download?.()"
      ></button>
      <button
        class="fa-solid fa-clock-rotate-left"
        aria-label="File history"
        @click="history?.()"
      ></button>
    </div>
    <div class="file-drawer">
      <div
        v-for="file in props.files"
        :key="file.name"
        role="button"
        :aria-label="`Open ${file.name}`"
        class="file"
        :class="{ selected: file.name === props.currentFile }"
        @click="selectFile?.(file.name)"
      >
        {{ file.name }}
      </div>
    </div>
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

.sidebar-title {
  padding: 0.5em;
  text-align: center;
  font-weight: bold;
  font-style: italic;
  border-bottom: 1pt dotted;
}

.sidebar-account {
  padding: 0.4em 0.5em;
  border-bottom: 1pt dotted;
  font-size: 0.85em;
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
  padding: 0.25em;
  display: flex;
  justify-content: center;
  align-items: center;
  gap: 0.25em;
}

.file-drawer {
  display: flex;
  flex-direction: column;
  padding: 0.5em;
  flex: 1;
  overflow-x: hidden;
  overflow-y: scroll;
}

.file {
  padding: 0.25em;
  cursor: pointer;
}

.file.selected {
  background: var(--accent);
  color: var(--accent-fg);
}
</style>
