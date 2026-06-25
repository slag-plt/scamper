<script setup lang="ts">
import { onMounted, onUnmounted, ref, shallowRef } from "vue"
import { Pane, Splitpanes } from "splitpanes"
import "splitpanes/dist/splitpanes.css"
import * as Lock from "../lockfile"
import { initializeLibs } from "../../lib"
import IdeSidebar from "./IdeSidebar.vue"
import IdeHeader from "./IdeHeader.vue"
import ResultsPane from "./ResultsPane.vue"
import CodeMirrorEditor from "./CodeMirrorEditor.vue"
import QueryModal from "./query/QueryModal.vue"
import { provideEditor } from "./editor-context"
import type { ResultsPaneType } from "./use-results-pane"
import { ScamperInstance } from "../../scamper-instance"
import { ErrorChannel, Loc, OutputChannel, ReportError } from "../../lpm"
import * as FS from "../../fs"
import { FileEntry } from "../../fs/fs"
import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import ValueRenderer from "../../lpm/renderers/vue/ValueRenderer.vue"

// ---------- config ----------

const CONFIG_FILENAME = ".scamper.config"

interface Config {
  lastOpenedFilename: string | null
  lastVersionAccessed: string
}

const DEFAULT_CONFIG: Config = {
  lastOpenedFilename: null,
  lastVersionAccessed: "0.0.0",
}

const appVersion = `(${APP_VERSION})`

// ---------- mutable IDE state (non-reactive where not needed in template) ----

let fs: FS.t | null = null
const scamperInstance: ScamperInstance = ScamperInstance.getInstance()
let autosaveId = -1
let config: Config = DEFAULT_CONFIG
let isLoadingFile = false

// ---------- reactive state ----------

const currentFile = ref<string | null>(null)
const isDirty = ref(false)
const isTracing = ref(false)
const files = ref<FileEntry[]>([])
const isSidebarVisible = ref(true)
const isLoading = ref(true)
const loadingContent = ref("Loading Scamper...")

// ---------- editor context + child component refs ----------

const editor = provideEditor()
const resultsRef = shallowRef<ResultsPaneType | null>(null)

interface QueryEntry {
  id: number
  targetPos: number
  rep: SimpleErrorChannel
}
const queries = ref<QueryEntry[]>([])
let nextQueryId = 0

// ---------- file drawer ----------

async function populateFileDrawer() {
  if (!fs) throw new Error("FileSystem not initialized")
  const allFiles = await fs.getFileList()
  files.value = allFiles.filter(
    (f: FileEntry) => !f.isDirectory && !f.name.startsWith("."),
  )
}

// ---------- config persistence ----------

async function saveConfig() {
  await fs?.saveFile(CONFIG_FILENAME, JSON.stringify(config))
}

async function loadConfig() {
  if (!fs) return
  if (await fs.fileExists(CONFIG_FILENAME)) {
    config = JSON.parse(await fs.loadFile(CONFIG_FILENAME)) as Config
  } else {
    config = DEFAULT_CONFIG
    await saveConfig()
  }
}

// ---------- autosave ----------

function startAutosaving() {
  if (autosaveId === -1) {
    autosaveId = window.setInterval(() => {
      void saveCurrentFile()
    }, 3000)
  }
}

function stopAutosaving() {
  window.clearInterval(autosaveId)
  autosaveId = -1
}

// ---------- dirty tracking ----------

function makeDirty() {
  isDirty.value = true
  queries.value = []
}

function makeClean() {
  isDirty.value = false
}

// ---------- scamper execution ----------

interface ScamperExecutionProps {
  tracing: boolean
  err?: ErrorChannel
  out?: OutputChannel
  queryLoc?: Loc | null
}
function executeScamper({
  tracing,
  err = resultsRef.value?.display,
  out = resultsRef.value?.display,
  queryLoc = null,
}: ScamperExecutionProps) {
  if (!err || !out) return
  resultsRef.value?.reset()
  isTracing.value = true
  const src = editor().getDoc()
  if (queryLoc) {
    scamperInstance.query({ src, rep: err, queryLoc })
  } else {
    scamperInstance.execute({
      src,
      err,
      out,
      isTracing: tracing,
    })
  }
  makeClean()
}

// ---------- file operations ----------

async function saveCurrentFile() {
  if (!currentFile.value || !fs) return
  try {
    await fs.saveFile(currentFile.value, editor().getDoc())
  } catch (e) {
    if (e instanceof Error) displayError(e.message)
  }
}

async function switchToFile(filename: string): Promise<void> {
  if (!fs) return
  isLoadingFile = true
  stopAutosaving()
  if (currentFile.value !== null) await saveCurrentFile()

  currentFile.value = filename
  try {
    const src = await fs.loadFile(currentFile.value)
    editor().initializeDoc(src)
  } catch (e) {
    if (e instanceof Error) displayError(`${e.message}\n\n${e.stack ?? ""}`)
  }

  resultsRef.value?.reset()
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

function handleRun() {
  executeScamper({ tracing: false })
}

function handleTrace() {
  executeScamper({ tracing: true })
}

function handleCancel() {
  // TODO: implement
}

async function handleRunWindow() {
  if (!currentFile.value) return
  await saveCurrentFile()
  const params = new URLSearchParams({
    filename: currentFile.value,
    isTree: "false",
  })
  window.open(`runner.html?${params.toString()}`)
}

function toggleSidebar() {
  isSidebarVisible.value = !isSidebarVisible.value
}

function handleQuery() {
  const rep = new SimpleErrorChannel()
  const loc = editor().getCursorLoc()
  queries.value.push({ id: nextQueryId++, targetPos: loc.idx, rep })
  executeScamper({
    tracing: false,
    err: rep,
    queryLoc: loc,
  })
}

function closeQuery(id: number) {
  queries.value = queries.value.filter((q) => q.id !== id)
}

// ---------- step handlers ----------

function handleStepOnce() {
  // TODO: implement
}

async function handleStepStmt() {
  // TODO: implement
}

async function handleStepAll() {
  // TODO: implement
}

// ---------- sidebar event handlers ----------

async function handleCreate() {
  const filename = prompt("Enter a file name for your new program.")
  if (filename === null) return
  if (await fs?.fileExists(filename)) {
    alert(`File ${filename} already exists!`)
  } else {
    await fs?.saveFile(filename, `; ${filename}`)
    await switchToFile(filename)
  }
}

async function handleUploadFile(file: File) {
  const content = await file.text()
  const filename = file.name
  if (await fs?.fileExists(filename)) {
    const ok = confirm(
      `File "${filename}" already exists. Do you want to overwrite it?`,
    )
    if (!ok) return
    stopAutosaving()
    await fs?.deleteFile(filename)
  }
  await fs?.saveFile(filename, content)
  currentFile.value = null
  await switchToFile(filename)
}

async function handleFileDrop(droppedFiles: FileList) {
  stopAutosaving()
  for (const file of droppedFiles) {
    try {
      const content = await file.text()
      const filename = file.name
      if (await fs?.fileExists(filename)) {
        const ok = confirm(
          `File "${filename}" already exists. Do you want to overwrite it?`,
        )
        if (!ok) continue
        stopAutosaving()
        await fs?.deleteFile(filename)
      }
      await fs?.saveFile(filename, content)
      currentFile.value = null
      await switchToFile(filename)
    } catch (e) {
      if (e instanceof Error)
        displayError(`Failed to upload file "${file.name}": ${e.message}`)
    }
  }
}

async function handleRename() {
  if (!currentFile.value) return
  const newName = prompt(`Enter a new filename for ${currentFile.value}`)
  if (newName === null || newName === currentFile.value) return
  if (await fs?.fileExists(newName)) {
    alert(`File ${newName} already exists!`)
  } else {
    try {
      stopAutosaving()
      // N.B., renaming closes the fs worker's handle to the current file,
      // so we load it fresh afterwards.
      await fs?.renameFile(currentFile.value, newName)
      currentFile.value = null
      await switchToFile(newName)
    } catch (e) {
      if (e instanceof Error) displayError(e.message)
    }
  }
}

async function handleDelete() {
  if (!currentFile.value) return
  const ok = confirm(`Are you sure you want to delete ${currentFile.value}?`)
  if (!ok) return
  stopAutosaving()
  await fs?.deleteFile(currentFile.value)
  currentFile.value = null
  editor().initializeDummyDoc()
  config.lastOpenedFilename = null
  resultsRef.value?.reset()
  await populateFileDrawer()
  startAutosaving()
}

async function handleDownload() {
  if (!currentFile.value || !fs) return
  const contents = await fs.loadFile(currentFile.value)
  const a = document.createElement("a")
  a.href = "data:attachment/text;charset=utf-8," + encodeURIComponent(contents)
  a.target = "_blank"
  a.download = currentFile.value
  a.click()
}

async function handleSelectFile(filename: string) {
  if (!isLoadingFile) await switchToFile(filename)
}

// ---------- page lifecycle handlers ----------

async function handleVisibilityChange() {
  if (document.visibilityState === "hidden") {
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

  const obtainedLock = await Lock.acquireLockFile(fs)
  if (!obtainedLock) {
    loadingContent.value =
      "Another instance of Scamper is open. Please close that instance and try again."
    return
  }

  document.addEventListener("visibilitychange", visibilityChangeWrapper)
  document.addEventListener("pagehide", pageHideWrapper)
  window.addEventListener("beforeunload", beforeUnloadWrapper)

  await initializeLibs()

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
})

onUnmounted(() => {
  stopAutosaving()
  document.removeEventListener("visibilitychange", visibilityChangeWrapper)
  document.removeEventListener("pagehide", pageHideWrapper)
  window.removeEventListener("beforeunload", beforeUnloadWrapper)
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
        :run="handleRun"
        :trace="handleTrace"
        :cancel="handleCancel"
        :query="handleQuery"
        @run-window="handleRunWindow"
        @toggle-sidebar="toggleSidebar"
      />
      <div class="content-area">
        <Splitpanes>
          <Pane :size="65" class="editor-pane">
            <CodeMirrorEditor @dirty="makeDirty" />
          </Pane>
          <Pane :size="35" class="results-pane">
            <ResultsPane
              ref="resultsRef"
              :is-dirty="isDirty"
              :is-tracing="isTracing"
              :step-once="handleStepOnce"
              :step-stmt="handleStepStmt"
              :step-all="handleStepAll"
              :cancel="handleCancel"
            />
          </Pane>
        </Splitpanes>
      </div>
    </div>
  </div>
  <div v-show="isLoading" class="loading">
    <div class="loading-content">{{ loadingContent }}</div>
  </div>
  <QueryModal
    v-for="q in queries"
    :key="q.id"
    :target-pos="q.targetPos"
    @close="closeQuery(q.id)"
  >
    <ValueRenderer
      v-for="[repI, repErr] in q.rep.errors
        .filter((e) => e instanceof ReportError)
        .entries()"
      :key="repI"
      :value="repErr.value"
    />
  </QueryModal>
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
  border-right: 1px solid #ddd;
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
  background-color: white;
  overflow: hidden;
}

.results-pane {
  background-color: white;
  display: flex;
  flex-direction: column;
}

:deep(.splitpanes__splitter) {
  background-color: #eee;
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
  background-color: rgba(0, 0, 0, 0.4);
}

.loading-content {
  background-color: #fefefe;
  margin: auto;
  padding: 20px;
  border: 1px solid #888;
  width: 80%;
}
</style>
