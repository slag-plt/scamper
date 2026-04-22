<script setup lang="ts">
import { ref } from "vue"
import { useIdeSidebar } from "./use-ide-sidebar"

const props = defineProps<{
  version?: string
  create?: () => void
  rename?: () => void
  deleteFile?: () => void
  download?: () => void
  archive?: () => void
  selectFile?: (filename: string) => void
  uploadFile?: (file: File) => Promise<void>
  fileDrop?: (files: FileList) => Promise<void>
}>()

const { files, currentFile, setFiles, setCurrentFile } = useIdeSidebar()

const isDragOver = ref(false)
const fileInputRef = ref<HTMLInputElement | null>(null)

function handleDragOver(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer) {
    event.dataTransfer.dropEffect = "copy"
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
  target.value = ""
}

defineExpose({ setFiles, setCurrentFile })
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
        aria-label="Download zip archive"
        disabled
        @click="archive?.()"
      ></button>
      ⋅
      <button
        class="fa-solid fa-pencil"
        aria-label="Rename file"
        :disabled="!currentFile"
        @click="rename?.()"
      ></button>
      <button
        class="fa-solid fa-trash"
        aria-label="Delete file"
        :disabled="!currentFile"
        @click="deleteFile?.()"
      ></button>
      <button
        class="fa-solid fa-download"
        aria-label="Download file"
        :disabled="!currentFile"
        @click="download?.()"
      ></button>
    </div>
    <div class="file-drawer">
      <div
        v-for="file in files"
        :key="file.name"
        role="button"
        :aria-label="`Open ${file.name}`"
        class="file"
        :class="{ selected: file.name === currentFile }"
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
  background: #f6f6f6;
}

.ide-sidebar.drag-over {
  opacity: 0.5;
  background: #e0e0e0;
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
  background: green;
  color: white;
}
</style>
