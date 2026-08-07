<script setup lang="ts">
import { onMounted, onUnmounted, ref, shallowRef } from 'vue'
import { Pane, Splitpanes } from 'splitpanes'
import 'splitpanes/dist/splitpanes.css'
import * as Lock from '../lockfile'
import IdeSidebar from './IdeSidebar.vue'
import IdeHeader from './IdeHeader.vue'
import ResultsPane from './ResultsPane.vue'
import CodeMirrorEditor from './CodeMirrorEditor.vue'
import IdeStatusBar from './IdeStatusBar.vue'
import type { CursorStatus } from '../codemirror/enclosing-form'
import { provideEditor } from '../composables/editor-context'
import type { ResultsPaneType } from '../composables/use-results-pane'
import { provideScamperSession } from '../composables/use-scamper-session'
import Scamper from '../../../scamper'
import * as FS from '../../../fs'
import { FileEntry } from '../../../fs/fs'
import { FileSession } from '../file-session'
import QueryGhostLine from './query/QueryGhostLine.vue'
import ExpandedQueryModal from './query/ExpandedQueryModal.vue'
import ModalHost from './ModalHost.vue'
import {
  modalAlert,
  modalConfirm,
  modalPrompt,
} from '../composables/use-modals'
import PatchNotesModal from './PatchNotesModal.vue'
import { compareVersions, patchNotesSince, type PatchNote } from '../patch-notes'

// ---------- config ----------

const CONFIG_FILENAME = '.scamper.config'

interface Config {
  lastOpenedFilename: string | null
  lastVersionAccessed: string
}

const DEFAULT_CONFIG: Config = {
  lastOpenedFilename: null,
  lastVersionAccessed: '0.0.0',
}

const appVersion = `(${APP_VERSION})`

// ---------- mutable IDE state (non-reactive where not needed in template) ----

let fs: FS.t | null = null
let fileSession: FileSession | null = null
let config: Config = { ...DEFAULT_CONFIG }
let isLoadingFile = false

// ---------- reactive state ----------

const currentFile = ref<string | null>(null)
const isDirty = ref(false)
const files = ref<FileEntry[]>([])
const isSidebarVisible = ref(true)
const isLoading = ref(true)
const loadingContent = ref('Loading Scamper...')
const cursorStatus = ref<CursorStatus>({ line: 1, column: 1, path: [] })
const patchNotesToShow = ref<PatchNote[]>([])
const showPatchNotes = ref(false)

// ---------- editor context + child component refs ----------

const editor = provideEditor()
const resultsRef = shallowRef<ResultsPaneType | null>(null)
const session = provideScamperSession(resultsRef, {
  editor,
  onRunScheduled: () => {
    isDirty.value = false
  },
})
const { isTracing, queries, expandedQueryId } = session

function abortTraceStep() {
  // Stop an in-flight statement/all burst, re-pausing the session (vs. stopRun,
  // which cancels the whole run).
  session.abortStep()
}

// ---------- file drawer ----------

async function populateFileDrawer() {
  if (!fs) throw new Error('FileSystem not initialized')
  const allFiles = await fs.getFileList()
  files.value = allFiles.filter(
    (f: FileEntry) => !f.isDirectory && !f.name.startsWith('.'),
  )
}

// ---------- config persistence ----------

async function saveConfig() {
  await fs?.saveFile(CONFIG_FILENAME, JSON.stringify(config))
}

async function loadConfig() {
  if (!fs) return
  if (await fs.fileExists(CONFIG_FILENAME)) {
    // Merge over the defaults so a config written by an older build (missing a
    // newer field) still loads with sane values.
    const stored = JSON.parse(await fs.loadFile(CONFIG_FILENAME)) as Partial<Config>
    config = { ...DEFAULT_CONFIG, ...stored }
  } else {
    // A brand-new user starts already "caught up" to the current version, so
    // they aren't greeted with a backlog of patch notes.
    config = { ...DEFAULT_CONFIG, lastVersionAccessed: APP_VERSION }
    await saveConfig()
  }
}

// Records the current version as seen so its patch notes aren't shown again.
// Only ever moves forward, so running an older build never rewinds the seen
// version (which would re-show notes on a later re-upgrade).
async function markVersionSeen() {
  if (compareVersions(config.lastVersionAccessed, APP_VERSION) < 0) {
    config.lastVersionAccessed = APP_VERSION
    await saveConfig()
  }
}

// Shows patch notes for any versions the user hasn't seen yet. The version is
// recorded as seen as soon as the notes are shown (not on dismissal), so a user
// who closes the tab without clicking through still isn't shown them again.
async function showPatchNotesIfNeeded() {
  const unseen = patchNotesSince(config.lastVersionAccessed, APP_VERSION)
  await markVersionSeen()
  if (unseen.length > 0) {
    patchNotesToShow.value = unseen
    showPatchNotes.value = true
  }
}

function handlePatchNotesClose() {
  showPatchNotes.value = false
  patchNotesToShow.value = []
}

// ---------- autosave ----------

function startAutosaving() {
  fileSession?.startAutosave()
}

function stopAutosaving() {
  fileSession?.stopAutosave()
}

// ---------- dirty tracking ----------

function makeDirty() {
  isDirty.value = true
  session.invalidateAllQueries()
}

function handleCursorChange(status: CursorStatus) {
  cursorStatus.value = status
}

// ---------- file operations ----------

function isEditorLoaded(): boolean {
  try {
    return editor().isLoaded()
  } catch {
    return false
  }
}

async function saveCurrentFile() {
  if (!fileSession || isLoadingFile) return
  await fileSession.save()
}

// Keeps the reactive `currentFile` ref (read by the template) in sync with the
// file session's notion of the open file (used by its race-safe save/delete).
function setCurrentFile(filename: string | null) {
  currentFile.value = filename
  fileSession?.setCurrentFile(filename)
}

async function switchToFile(filename: string): Promise<void> {
  if (!fs || !fileSession) return
  isLoadingFile = true
  stopAutosaving()
  session.stopAll()

  try {
    // Forces a save of the outgoing file before loading the new one so a quick
    // edit is never lost on switch (issue #238). The guarded saveCurrentFile()
    // would no-op here because isLoadingFile is already set.
    const src = await fileSession.switchTo(filename)
    currentFile.value = filename
    editor().initializeDoc(src)
  } catch (e) {
    if (e instanceof Error) displayError(`${e.message}\n\n${e.stack ?? ''}`)
  }

  session.resetOutput()
  await populateFileDrawer()
  config.lastOpenedFilename = currentFile.value
  startAutosaving()
  isLoadingFile = false
}

function displayError(error: string) {
  loadingContent.value = error
  isLoading.value = true
}

// ---------- header event handlers ----------

async function handleRunWindow() {
  if (!currentFile.value) return
  await saveCurrentFile()
  const params = new URLSearchParams({
    filename: currentFile.value,
    isTree: 'false',
  })
  window.open(`runner.html?${params.toString()}`)
}

function toggleSidebar() {
  isSidebarVisible.value = !isSidebarVisible.value
}

// ---------- step handlers ----------

function handleStepOnce() {
  session.step()
}

async function handleStepStmt() {
  await session.stepStmt()
}

async function handleStepAll() {
  await session.stepAll()
}

// ---------- sidebar event handlers ----------

async function handleCreate() {
  const filename = await modalPrompt({
    title: 'New file',
    message: 'Enter a file name for your new program.',
  })
  if (filename === null) return
  if (await fs?.fileExists(filename)) {
    await modalAlert({ message: `File ${filename} already exists!` })
  } else {
    await fs?.saveFile(filename, `; ${filename}`)
    await switchToFile(filename)
  }
}

async function handleUploadFile(file: File) {
  if (!fs || !fileSession) return
  const content = await file.text()
  const filename = file.name
  if (await fs.fileExists(filename)) {
    const ok = await modalConfirm({
      title: 'Overwrite file',
      message: `File "${filename}" already exists. Do you want to overwrite it?`,
      confirmLabel: 'Overwrite',
    })
    if (!ok) return
    // Serialize the overwrite against any in-flight save so the writable is
    // closed before the file is removed (see file-session.ts).
    await fileSession.deleteFile(filename, { replacing: true })
  }
  await fs.saveFile(filename, content)
  setCurrentFile(null)
  await switchToFile(filename)
}

async function handleFileDrop(droppedFiles: FileList) {
  if (!fs || !fileSession) return
  stopAutosaving()
  for (const file of droppedFiles) {
    try {
      const content = await file.text()
      const filename = file.name
      if (await fs.fileExists(filename)) {
        const ok = await modalConfirm({
          title: 'Overwrite file',
          message: `File "${filename}" already exists. Do you want to overwrite it?`,
          confirmLabel: 'Overwrite',
        })
        if (!ok) continue
        // Serialize the overwrite against any in-flight save (see above).
        await fileSession.deleteFile(filename, { replacing: true })
      }
      await fs.saveFile(filename, content)
      setCurrentFile(null)
      await switchToFile(filename)
    } catch (e) {
      if (e instanceof Error)
        displayError(`Failed to upload file "${file.name}": ${e.message}`)
    }
  }
}

async function handleRename() {
  if (!currentFile.value || !fileSession) return
  const from = currentFile.value
  const newName = await modalPrompt({
    title: 'Rename file',
    message: `Enter a new filename for ${from}`,
    defaultValue: from,
  })
  if (newName === null || newName === from) return
  if (await fs?.fileExists(newName)) {
    await modalAlert({ message: `File ${newName} already exists!` })
  } else {
    try {
      // N.B., renaming closes the fs worker's handle to the current file,
      // so we load it fresh afterwards. The session serializes against any
      // in-flight save first.
      await fileSession.renameFile(from, newName)
      setCurrentFile(null)
      await switchToFile(newName)
    } catch (e) {
      if (e instanceof Error) displayError(e.message)
    }
  }
}

async function handleDelete() {
  if (!currentFile.value || !fileSession) return
  const target = currentFile.value
  const ok = await modalConfirm({
    title: 'Delete file',
    message: `Are you sure you want to delete ${target}?`,
    confirmLabel: 'Delete',
    danger: true,
  })
  if (!ok) return
  try {
    // The session stops autosave and awaits any in-flight save before removing
    // the file, so an open OPFS writable can't block the delete (issue #184).
    await fileSession.deleteFile(target)
  } catch (e) {
    if (e instanceof Error) displayError(`Failed to delete ${target}: ${e.message}`)
    return
  }
  setCurrentFile(null)
  editor().initializeDummyDoc()
  config.lastOpenedFilename = null
  session.stopAll()
  session.resetOutput()
  await populateFileDrawer()
  startAutosaving()
}

async function handleDownload() {
  if (!currentFile.value || !fs) return
  const contents = await fs.loadFile(currentFile.value)
  const a = document.createElement('a')
  a.href = 'data:attachment/text;charset=utf-8,' + encodeURIComponent(contents)
  a.target = '_blank'
  a.download = currentFile.value
  a.click()
}

async function handleSelectFile(filename: string) {
  if (!isLoadingFile) await switchToFile(filename)
}

// ---------- page lifecycle handlers ----------

async function handleVisibilityChange() {
  if (document.visibilityState === 'hidden') {
    await saveCurrentFile()
    await saveConfig()
    if (fs) await Lock.releaseLockFile(fs)
  } else {
    if (fs) await Lock.acquireLockFile(fs)
  }
}

async function handlePageHide() {
  await saveCurrentFile()
  await saveConfig()
  if (fs) await Lock.releaseLockFile(fs)
}

async function handleBeforeUnload(e: BeforeUnloadEvent) {
  await saveCurrentFile()
  await saveConfig()
  if (fs) await Lock.releaseLockFile(fs)
  if (isDirty.value) {
    e.preventDefault()
  }
}

// Stable wrapper refs so removeEventListener can match the same function objects.
const visibilityChangeWrapper = () => {
  void handleVisibilityChange()
}
const pageHideWrapper = () => {
  void handlePageHide()
}
const beforeUnloadWrapper = (e: Event) => {
  void handleBeforeUnload(e as BeforeUnloadEvent)
}

// ---------- lifecycle ----------

onMounted(async () => {
  await FS.initialize()
  fs = FS.getFS()
  fileSession = new FileSession(
    fs,
    { getDoc: () => editor().getDoc(), isEditorLoaded },
    {
      onSaveError: (message) => {
        displayError(message)
      },
    },
  )

  const obtainedLock = await Lock.acquireLockFile(fs)
  if (!obtainedLock) {
    loadingContent.value =
      'Another instance of Scamper is open. Please close that instance and try again.'
    return
  }

  document.addEventListener('visibilitychange', visibilityChangeWrapper)
  document.addEventListener('pagehide', pageHideWrapper)
  window.addEventListener('beforeunload', beforeUnloadWrapper)

  await loadConfig()
  await populateFileDrawer()

  if (config.lastOpenedFilename !== null) {
    if (await fs.fileExists(config.lastOpenedFilename)) {
      // TODO: re-enable once we have a better handle on large-file loading
      // await switchToFile(config.lastOpenedFilename)
    } else {
      config.lastOpenedFilename = null
    }
  }

  isLoading.value = false
  Scamper.getInstance().calibrateScheduler()

  await showPatchNotesIfNeeded()
})

onUnmounted(() => {
  stopAutosaving()
  session.stopAll()
  document.removeEventListener('visibilitychange', visibilityChangeWrapper)
  document.removeEventListener('pagehide', pageHideWrapper)
  window.removeEventListener('beforeunload', beforeUnloadWrapper)
})
</script>

<template>
  <div class="ide-app">
    <div v-show="isSidebarVisible" class="sidebar-wrapper">
      <IdeSidebar
        :version="appVersion"
        :files="files"
        :current-file="currentFile"
        :create="handleCreate"
        :rename="handleRename"
        :delete-file="handleDelete"
        :download="handleDownload"
        :select-file="handleSelectFile"
        :upload-file="handleUploadFile"
        :file-drop="handleFileDrop"
      />
    </div>
    <div class="ide-main">
      <IdeHeader
        :current-file="currentFile"
        @run-window="handleRunWindow"
        @toggle-sidebar="toggleSidebar"
      />
      <div class="content-area">
        <Splitpanes>
          <Pane :size="65" class="editor-pane">
            <CodeMirrorEditor @dirty="makeDirty" @cursor-change="handleCursorChange" />
          </Pane>
          <Pane :size="35" class="results-pane">
            <ResultsPane
              ref="resultsRef"
              :is-dirty="isDirty"
              :is-tracing="isTracing"
              :step-once="handleStepOnce"
              :step-stmt="handleStepStmt"
              :step-all="handleStepAll"
              :abort-step="abortTraceStep"
            />
          </Pane>
        </Splitpanes>
      </div>
      <IdeStatusBar
        :line="cursorStatus.line"
        :column="cursorStatus.column"
        :path="cursorStatus.path"
      />
    </div>
  </div>
  <div v-show="isLoading" class="loading">
    <div class="loading-content">{{ loadingContent }}</div>
  </div>
  <QueryGhostLine
    v-for="[line, qs] in queries"
    :key="line"
    :line="line"
    :queries="qs"
  />
  <ExpandedQueryModal
    v-if="expandedQueryId !== null"
    :query-id="expandedQueryId"
  />
  <ModalHost />
  <PatchNotesModal
    :open="showPatchNotes"
    :notes="patchNotesToShow"
    @close="handlePatchNotesClose"
  />
</template>

<style scoped>
.ide-app {
  display: flex;
  flex-direction: row;
  height: 100%;
}

.sidebar-wrapper {
  width: 250px;
  flex-shrink: 0;
  border-right: 1px solid var(--border-muted);
}

.ide-main {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
}

.content-area {
  flex: 1;
  min-height: 0;
  position: relative;
}

.editor-pane {
  background-color: var(--surface);
  overflow: hidden;
}

.results-pane {
  background-color: var(--surface);
  display: flex;
  flex-direction: column;
}

:deep(.splitpanes__splitter) {
  background-color: var(--splitter-bg);
  background-image: url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAeCAYAAADkftS9AAAAIklEQVQoU2M4c+bMfxAGAgYYmwGrIIiDjrELjpo5aiZeMwF+yNnOs5KSvgAAAABJRU5ErkJggg==");
  background-repeat: no-repeat;
  background-position: 50%;
  cursor: col-resize;
  width: 10px;
  flex-shrink: 0;
}

.loading {
  position: fixed;
  z-index: 1;
  padding-top: 100px;
  left: 0;
  top: 0;
  width: 100%;
  height: 100%;
  overflow: auto;
  background-color: var(--overlay);
}

.loading-content {
  background-color: var(--modal-bg);
  margin: auto;
  padding: 20px;
  border: 1px solid var(--modal-border);
  width: 80%;
}
</style>
