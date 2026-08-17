<script setup lang="ts">
import { onMounted, onUnmounted, ref, shallowRef } from 'vue'
import { Pane, Splitpanes } from 'splitpanes'
import 'splitpanes/dist/splitpanes.css'
import * as SingleInstance from '../single-instance'
import {
  LEGACY_CONFIG_FILENAME,
  MAX_RECENT_FILES,
  readStoredConfig,
  writeStoredConfig,
  type Config,
} from '../ide-config'
import IdeSidebar from './IdeSidebar.vue'
import IdeMenuBar from './IdeMenuBar.vue'
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
import { FileEntry, isUserFile } from '../../../fs/fs'
import { FileSession } from '../file-session'
import { archiveFilename, buildArchive } from '../archive'
import QueryGhostLine from './query/QueryGhostLine.vue'
import ExpandedQueryModal from './query/ExpandedQueryModal.vue'
import ModalHost from './ModalHost.vue'
import {
  modalAlert,
  modalConfirm,
  modalPrompt,
} from '../composables/use-modals'
import PatchNotesModal from './PatchNotesModal.vue'
import FileHistoryModal from './FileHistoryModal.vue'
import SignInModal from './SignInModal.vue'
import { restart, serverSession, usesServerFiles } from '../server-session'
import { signInWithPassword, signOut } from '../auth-client'
import * as Connectivity from '../connectivity'
import { isNotSignedIn } from '../../../fs/session'
import { isUnreachable } from '../../../fs/unreachable'
import { importLocalFiles, localFileNames } from '../local-import'
import type { History, HistoryFile, SnapshotRef } from '../../../history'
import {
  compareVersions,
  patchNotes,
  patchNotesSince,
  type PatchNote,
} from '../patch-notes'

// ---------- config ----------

const OTHER_INSTANCE_MESSAGE =
  'Another instance of Scamper is open. Please close that instance and try again.'

const DEFAULT_CONFIG: Config = {
  lastOpenedFilename: null,
  lastVersionAccessed: '0.0.0',
  localFilesOffered: false,
  recentFiles: [],
}

const appVersion = `(${APP_VERSION})`

// ---------- mutable IDE state (non-reactive where not needed in template) ----

let fs: FS.t | null = null
let fileHistory: History | null = null
let fileSession: FileSession | null = null
let config: Config = { ...DEFAULT_CONFIG }
let isLoadingFile = false
// Unsubscribes the connection watcher; null until the IDE mounts.
let stopWatchingConnection: (() => void) | null = null

// ---------- reactive state ----------

const currentFile = ref<string | null>(null)
const isDirty = ref(false)
const files = ref<FileEntry[]>([])
// Recently opened files that still exist and aren't the one already open.
const recentFiles = ref<string[]>([])
const isSidebarVisible = ref(true)
const isLoading = ref(true)
const loadingContent = ref('Loading Scamper...')
const cursorStatus = ref<CursorStatus>({ line: 1, column: 1, path: [] })
const patchNotesToShow = ref<PatchNote[]>([])
const showPatchNotes = ref(false)
const showHistory = ref(false)
const historyFiles = ref<HistoryFile[]>([])
const historyFile = ref('')
const historySnapshots = ref<SnapshotRef[]>([])
const historyContents = ref<string | null>(null)
// The selected version's contents, fetched only when a version is picked. A
// history may hold fifty versions of a file; the browser needs one at a time.
const historySelected = ref<string | null>(null)

// ---------- signing in to the file server (#357) ----------

// Null where the deployment advertises no server, which is the common case and
// the one where none of this appears.
const fileServer = serverSession()
const signInMethods = fileServer?.methods ?? { password: false }
const hasServer = fileServer !== null
const canSignIn = signInMethods.password
// Whether *this tab's* files are the server's. A signed-out user on a
// server-backed deployment keeps their files in the browser, so the server
// going away costs them nothing and must not block anything.
const filesOnServer = usesServerFiles()
const connection = Connectivity.connection
const signedInAs = ref<string | null>(fileServer?.user?.email ?? null)
const showSignIn = ref(false)
const signInBusy = ref(false)
const signInError = ref<string | null>(null)
// Set when a session ended mid-edit rather than the user asking to sign in.
// Dismissing then has to restart, because the file system this tab is holding
// answers 401 to everything and there is no way back to it without a session.
const sessionLapsed = ref(false)

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
  files.value = allFiles.filter(isUserFile)
  syncRecentFiles()
}

/** Moves `filename` to the front of the recent list, trimming it to length. */
function noteRecentFile(filename: string) {
  config.recentFiles = [
    filename,
    ...config.recentFiles.filter((f) => f !== filename),
  ].slice(0, MAX_RECENT_FILES)
  saveConfig()
  syncRecentFiles()
}

/**
 * Republishes the recent list for the File menu, dropping the file already
 * open and any that no longer exist -- renamed, deleted, or belonging to an
 * account this browser is now signed out of. A menu item that opens nothing
 * would be worse than a shorter menu.
 */
function syncRecentFiles() {
  const present = new Set(files.value.map((f) => f.name))
  recentFiles.value = config.recentFiles.filter(
    (name) => name !== currentFile.value && present.has(name),
  )
}

// ---------- config persistence ----------

function saveConfig() {
  writeStoredConfig(config)
}

/**
 * Reads the config an older build kept in the file system, removing it so this
 * happens once. Without it, every existing user would look brand new.
 * @returns the stored config, or null if there wasn't one
 */
async function takeLegacyConfig(): Promise<Partial<Config> | null> {
  if (!fs) return null
  try {
    if (!(await fs.fileExists(LEGACY_CONFIG_FILENAME))) return null
    const raw = await fs.loadFile(LEGACY_CONFIG_FILENAME)
    await fs.deleteFile(LEGACY_CONFIG_FILENAME)
    return JSON.parse(raw) as Partial<Config>
  } catch {
    // A leftover we can't read is one we can do without.
    return null
  }
}

async function loadConfig() {
  const stored = readStoredConfig() ?? (await takeLegacyConfig())
  config =
    stored === null
      ? // A brand-new user starts already "caught up" to the current version,
        // so they aren't greeted with a backlog of patch notes.
        { ...DEFAULT_CONFIG, lastVersionAccessed: APP_VERSION }
      : // Merge over the defaults so a config written by an older build
        // (missing a newer field) still loads with sane values.
        { ...DEFAULT_CONFIG, ...stored }
  saveConfig()
}

// Records the current version as seen so its patch notes aren't shown again.
// Only ever moves forward, so running an older build never rewinds the seen
// version (which would re-show notes on a later re-upgrade).
function markVersionSeen() {
  if (compareVersions(config.lastVersionAccessed, APP_VERSION) < 0) {
    config.lastVersionAccessed = APP_VERSION
    saveConfig()
  }
}

// Shows patch notes for any versions the user hasn't seen yet. The version is
// recorded as seen as soon as the notes are shown (not on dismissal), so a user
// who closes the tab without clicking through still isn't shown them again.
function showPatchNotesIfNeeded() {
  const unseen = patchNotesSince(config.lastVersionAccessed, APP_VERSION)
  markVersionSeen()
  if (unseen.length > 0) {
    patchNotesToShow.value = unseen
    showPatchNotes.value = true
  }
}

/**
 * Signs out, then restarts onto local storage.
 *
 * Matters most on a shared machine: without this a student cannot hand the
 * browser to the next person without leaving their files reachable. The save
 * first is deliberate -- signing out is not a reason to lose the last edit.
 */
async function handleSignOut() {
  if (fileServer === null) return
  await saveCurrentFile()
  await signOut(fileServer.client)
  restart()
}

function openSignIn() {
  signInError.value = null
  sessionLapsed.value = false
  showSignIn.value = true
}

/**
 * Closes the sign-in dialog. After a lapsed session that means restarting: this
 * tab holds a file system that answers 401 to everything, and starting over
 * lands on local storage, which at least works.
 */
function closeSignIn() {
  showSignIn.value = false
  if (sessionLapsed.value) restart()
}

async function handleSignInPassword(email: string, password: string) {
  if (fileServer === null) return
  signInBusy.value = true
  signInError.value = await signInWithPassword(
    fileServer.client,
    email,
    password,
  )
  signInBusy.value = false
  // The backend is chosen at startup, so start over rather than swapping the
  // file system out from under an open file. See server-session.ts.
  if (signInError.value === null) restart()
}

/**
 * Offers to copy this browser's files into the account, once per browser.
 *
 * Signing in swaps the file system, so anything saved here before there was an
 * account stops being listed -- which reads as loss even though nothing was
 * lost. Asked once and remembered either way: a student who says no should not
 * be asked again every time they open Scamper.
 */
async function offerLocalFiles() {
  if (fileServer?.user == null || config.localFilesOffered || !fs) return

  const names = await localFileNames()
  // Nothing to carry across, so nothing to ask about -- but record the offer as
  // made, since asking later would be about files from some other session.
  if (names.length === 0) {
    config.localFilesOffered = true
    saveConfig()
    return
  }

  const count = names.length.toString()
  const them = names.length === 1 ? 'it' : 'them'
  const files = names.length === 1 ? 'file' : 'files'
  const wanted = await modalConfirm({
    title: 'Copy your files into your account?',
    message:
      `You have ${count} ${files} saved in this browser from before you ` +
      `signed in. Copy ${them} into your account so ${them} follow you ` +
      `between computers?\n\nThe ${files} in this browser stay where they are.`,
    confirmLabel: 'Copy',
    cancelLabel: 'Not now',
  })

  // Recorded before the copy: if it fails, the answer to a repeated prompt on
  // every load is no better, and the files are still in the browser either way.
  config.localFilesOffered = true
  saveConfig()
  if (!wanted) return

  try {
    const result = await importLocalFiles(fs)
    await populateFileDrawer()
    if (result.renamed.length > 0) {
      const renamed = result.renamed
        .map((entry) => `${entry.from} -> ${entry.to}`)
        .join('\n')
      await modalAlert({
        title: 'Some copies were renamed',
        message:
          `Copied ${result.copied.length.toString()} files. Your account ` +
          `already had these names, so the copies came in as:\n\n${renamed}`,
      })
    }
  } catch (e) {
    reportError(e, (message) => `Could not copy your browser's files: ${message}`)
  }
}

/**
 * Shows this release's notes on request (Help > What's New), rather than only
 * once on first opening a new version. Falls back to the newest release that
 * has any, so the menu item is never a dead end on a version that shipped
 * without notes.
 */
async function handleWhatsNew() {
  const forThisVersion = patchNotes.filter((n) => n.version === APP_VERSION)
  const notes =
    forThisVersion.length > 0
      ? forThisVersion
      : [...patchNotes]
          .sort((a, b) => compareVersions(b.version, a.version))
          .slice(0, 1)
  if (notes.length === 0) {
    await modalAlert({
      title: "What's New",
      message: 'This version of Scamper has no release notes.',
    })
    return
  }
  patchNotesToShow.value = notes
  showPatchNotes.value = true
}

/** Help > About Scamper: what this is and which version is running. */
async function handleAbout() {
  await modalAlert({
    title: 'About Scamper',
    message:
      `Scamper ${APP_VERSION}\n\n` +
      'A mini-Scheme for teaching multimedia programming on the web, ' +
      'maintained by the Systems, Languages, and Automated Generation lab ' +
      'at Grinnell College.\n\n' +
      'github.com/slag-plt/scamper',
  })
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
    reportError(e, (message) => `${message}\n\n${e instanceof Error ? (e.stack ?? '') : ''}`)
  }

  session.resetOutput()
  config.lastOpenedFilename = currentFile.value
  if (currentFile.value !== null) noteRecentFile(currentFile.value)
  await populateFileDrawer()
  startAutosaving()
  isLoadingFile = false
}

/**
 * Reports an error to the person, telling the two recoverable ones apart from a
 * fault.
 *
 * A session can end mid-edit -- it expires, or an administrator resets the
 * password -- and the answer to that is to sign in again, not an error screen
 * over the editor. A server that cannot be reached is the same kind of thing:
 * wifi drops, laptops sleep, servers restart during a deploy, and none of that
 * is a reason to take the editor away from someone who is in the middle of
 * writing a program.
 *
 * @returns true iff the error was handled here
 */
function reportError(error: unknown, describe: (message: string) => string): boolean {
  if (isNotSignedIn(error)) {
    sessionLapsed.value = true
    signInError.value =
      'Your session ended, so that save did not reach the server. Sign in to carry on.'
    showSignIn.value = true
    return true
  }
  if (isUnreachable(error)) {
    // The request that just failed is better evidence than any heartbeat, so
    // the indicator flips now rather than up to a beat later.
    Connectivity.reportUnreachable()
    void announceOffline()
    return true
  }
  if (error instanceof Error) {
    displayError(describe(error.message))
    return true
  }
  return false
}

// Set while the offline notice is on screen, so a burst of failures -- an
// autosave and a file listing landing together -- produces one dismissable
// modal rather than a queue of identical ones.
let offlineAnnounced = false

/**
 * Says the server has gone away, once per outage, in a modal that closes.
 *
 * The warning about keeping the tab open is load-bearing: offline edits are
 * held in the editor and nowhere else. Mirroring a user's files client-side so
 * file operations keep working is issue #364; storing just the open buffer is
 * #363.
 */
async function announceOffline(): Promise<void> {
  if (offlineAnnounced) return
  offlineAnnounced = true
  await modalAlert({
    title: 'Scamper is offline',
    message:
      'Scamper cannot reach the server, so your files are out of reach for ' +
      'the moment.\n\nYou can keep writing and running code, and you can ' +
      'download the file you have open to keep a copy of it. Saving resumes ' +
      'on its own once the connection comes back — but until then, keep this ' +
      'tab open, because changes made while offline are not stored anywhere ' +
      'else.',
  })
}

/**
 * Refuses an action that needs the file server while it is out of reach.
 *
 * Dismissable, and it stops the action rather than letting it half-happen:
 * offline there is nowhere to write, so a delete that "succeeded" locally would
 * be a lie the next listing corrects.
 *
 * @param action what was being attempted, e.g. "Renaming a file"
 * @returns true iff the action may go ahead
 */
async function requireServer(action: string): Promise<boolean> {
  if (!filesOnServer || connection.value === 'online') return true
  await modalAlert({
    title: 'Scamper is offline',
    message:
      `${action} needs the Scamper server, which cannot be reached right ` +
      'now. Check your connection and try again — your open file is still ' +
      'here in the editor.',
  })
  return false
}

function displayError(error: string) {
  loadingContent.value = error
  isLoading.value = true
}

// ---------- header event handlers ----------

async function handleRunWindow() {
  if (!currentFile.value) return
  // The runner window loads the file from storage rather than from this
  // editor, so offline it would open onto nothing.
  if (!(await requireServer('Opening the run window'))) return
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
  if (!(await requireServer('Creating a file'))) return
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

/**
 * Asks for a file and uploads it. The picker is made here rather than kept in
 * the markup because two places offer an upload -- the drawer and the File
 * menu -- and one transient input is simpler than a hidden one per caller.
 */
function handlePickUpload() {
  const input = document.createElement('input')
  input.type = 'file'
  input.addEventListener('change', () => {
    const file = input.files?.[0]
    if (file) void handleUploadFile(file)
  })
  input.click()
}

async function handleUploadFile(file: File) {
  if (!fs || !fileSession) return
  if (!(await requireServer('Uploading a file'))) return
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
  if (!(await requireServer('Uploading files'))) return
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

/**
 * Saves the open file now, rather than waiting for the next autosave tick.
 *
 * Scamper saves on its own, so this exists because File > Save is where a
 * student will look when they want to be sure -- and because `save` declines
 * silently when there is nowhere to write, which for an action someone
 * deliberately picked should say so instead.
 */
async function handleSave() {
  if (!fileSession || currentFile.value === null) return
  if (!(await requireServer('Saving a file'))) return
  await fileSession.save({ force: true })
}

/**
 * Asks before a rename destroys the recoverable history of a deleted file that
 * used to hold `name`.
 * @returns true if the rename should go ahead
 */
async function confirmDiscardingHistory(name: string): Promise<boolean> {
  if (!fileHistory) return true
  const existing = await fileHistory.index(name)
  if (existing.deletedAt === undefined || existing.snapshots.length === 0) {
    return true
  }
  return modalConfirm({
    title: 'Discard saved history',
    message:
      `A deleted file named "${name}" still has ${existing.snapshots.length.toString()} ` +
      'saved versions you could recover. Using that name discards them.',
    confirmLabel: 'Rename anyway',
    danger: true,
  })
}

async function handleRename(from: string) {
  if (!fileSession) return
  if (!(await requireServer('Renaming a file'))) return
  const newName = await modalPrompt({
    title: 'Rename file',
    message: `Enter a new filename for ${from}`,
    defaultValue: from,
  })
  if (newName === null || newName === from) return
  if (await fs?.fileExists(newName)) {
    await modalAlert({ message: `File ${newName} already exists!` })
  } else {
    // A deleted file's history is kept under its old name so it can be
    // recovered (#42), and renaming onto that name overwrites it. Ask first,
    // the way an upload asks before overwriting a file -- silently discarding
    // the one thing that made a deleted file recoverable is the worst
    // outcome here.
    if (!(await confirmDiscardingHistory(newName))) return
    const wasOpen = from === currentFile.value
    try {
      // N.B., renaming closes the fs worker's handle to the file, so an open
      // one is loaded fresh afterwards. The session serializes against any
      // in-flight save first.
      await fileSession.renameFile(from, newName)
    } catch (e) {
      if (e instanceof Error) displayError(e.message)
      return
    }
    if (wasOpen) {
      setCurrentFile(null)
      await switchToFile(newName)
    } else {
      await populateFileDrawer()
      // The rename stopped autosave to serialize against an in-flight save;
      // nothing about the open file changed, so put it back.
      startAutosaving()
    }
  }
}

async function handleDelete(target: string) {
  if (!fileSession) return
  if (!(await requireServer('Deleting a file'))) return
  const ok = await modalConfirm({
    title: 'Delete file',
    message: `Are you sure you want to delete ${target}?`,
    confirmLabel: 'Delete',
    danger: true,
  })
  if (!ok) return
  const wasOpen = target === currentFile.value
  try {
    // The session stops autosave and awaits any in-flight save before removing
    // the file, so an open OPFS writable can't block the delete (issue #184).
    await fileSession.deleteFile(target)
  } catch (e) {
    if (e instanceof Error) displayError(`Failed to delete ${target}: ${e.message}`)
    return
  }
  // Deleting some other file leaves the editor alone: what is open is still
  // open, and stopping its run would be a surprise.
  if (wasOpen) closeOpenFile()
  await populateFileDrawer()
  startAutosaving()
}

/**
 * Returns the IDE to its no-file state: nothing open, an empty editor, and no
 * run left over from the file that was. Shared by delete and File > Close File,
 * which differ only in whether the file itself survives.
 */
function closeOpenFile() {
  setCurrentFile(null)
  editor().initializeDummyDoc()
  config.lastOpenedFilename = null
  saveConfig()
  session.stopAll()
  session.resetOutput()
  isDirty.value = false
}

/** Closes the open file, saving it first so nothing typed is lost. */
async function handleCloseFile() {
  if (currentFile.value === null) return
  // Best effort, exactly like the download: offline this declines by itself
  // rather than refusing to close, since the file stays on disk either way.
  await saveCurrentFile()
  closeOpenFile()
}

/**
 * Copies `source` to a name the user picks.
 * @param options.open whether to switch to the copy (File > Save As) or leave
 *        the current file open (the drawer's Duplicate).
 * @param options.title what the prompt calls itself.
 */
async function copyFileTo(
  source: string,
  options: { open: boolean; title: string },
) {
  if (!fs || !fileSession) return
  if (!(await requireServer(options.title))) return
  // The editor holds the newest contents of the open file; any other file is
  // read from storage.
  const contents =
    source === currentFile.value && isEditorLoaded()
      ? editor().getDoc()
      : await fs.loadFile(source)

  const newName = await modalPrompt({
    title: options.title,
    message: `Enter a name for the copy of ${source}`,
    defaultValue: suggestCopyName(source),
  })
  if (newName === null || newName === source) return
  if (await fs.fileExists(newName)) {
    await modalAlert({ message: `File ${newName} already exists!` })
    return
  }
  // Writing over a deleted file's name discards the history that made it
  // recoverable, the same way a rename onto it would (#42).
  if (!(await confirmDiscardingHistory(newName))) return

  try {
    await fs.saveFile(newName, contents)
  } catch (e) {
    reportError(e, (message) => `Could not copy ${source}: ${message}`)
    return
  }
  if (options.open) {
    await switchToFile(newName)
  } else {
    await populateFileDrawer()
  }
}

/**
 * Suggests "foo-copy.scm" for "foo.scm", so the prompt opens on a name that is
 * already free rather than on one that is taken.
 */
function suggestCopyName(source: string): string {
  const dot = source.lastIndexOf('.')
  return dot <= 0
    ? `${source}-copy`
    : `${source.slice(0, dot)}-copy${source.slice(dot)}`
}

function handleSaveAs() {
  const file = currentFile.value
  if (file === null) return
  // Scamper autosaves, so the original is already up to date on disk and "save
  // as" is really "make a copy and work on that" -- which is what it does.
  void copyFileTo(file, { open: true, title: 'Save as' })
}

function handleDuplicate(target: string) {
  void copyFileTo(target, { open: false, title: 'Duplicate file' })
}

// How long a generated object URL is kept alive after its download starts.
// Generous on purpose: the cost of holding a zip a few seconds too long is a
// little memory, while releasing it too early loses the download.
const DOWNLOAD_URL_LIFETIME_MS = 10_000

/** Hands `href` to the browser as a download named `filename`. */
function startDownload(filename: string, href: string) {
  const a = document.createElement('a')
  a.href = href
  a.target = '_blank'
  a.download = filename
  a.click()
}

/** Hands `contents` to the browser as a download named `filename`. */
function startTextDownload(filename: string, contents: string) {
  startDownload(
    filename,
    'data:attachment/text;charset=utf-8,' + encodeURIComponent(contents),
  )
}

/**
 * Downloads `target`: the open file as it appears in the editor, any other file
 * as it is stored.
 *
 * The open file comes from the editor rather than from storage, which differs
 * in two ways and both are improvements. Online it hands over what the student
 * is looking at instead of the last version that reached the server -- which is
 * what "download" was always taken to mean. Offline it works at all: everything
 * else that touches a file is refused, so this is the only way to get work off a
 * machine that cannot save. A stopgap until a signed-in user's files are
 * mirrored locally (#364), and deliberately not extended to the zip export or to
 * the other files here, both of which have to read storage and so genuinely need
 * the server.
 */
async function handleDownload(target: string) {
  if (target !== currentFile.value || !isEditorLoaded()) {
    if (!fs) return
    if (!(await requireServer('Downloading a file'))) return
    try {
      startTextDownload(target, await fs.loadFile(target))
    } catch (e) {
      reportError(e, (message) => `Could not download ${target}: ${message}`)
    }
    return
  }
  // Best effort, so the copy kept on the server matches the copy taken away.
  // It declines by itself while offline -- which is the case this exists for.
  await saveCurrentFile()
  startTextDownload(target, editor().getDoc())
}

// An archive can take a moment to build, and the button stays clickable while
// it does; the flag keeps a second click from starting a second export.
let isArchiving = false

// Downloads every file in the drawer as one zip archive (issue #42).
async function handleArchive() {
  if (!fs || isArchiving) return
  if (!(await requireServer('Exporting your files'))) return
  const ok = await modalConfirm({
    title: 'Export files',
    message: 'Download all of your files as a zip archive?',
    confirmLabel: 'Download',
  })
  if (!ok) return
  isArchiving = true
  try {
    // Pause autosave and let any in-flight write settle before reading every
    // file, the same way the other file operations serialize against the
    // session (see file-session.ts). Saving first also puts the edits still
    // sitting in the editor into the archive.
    stopAutosaving()
    await saveCurrentFile()
    const url = URL.createObjectURL(await buildArchive(fs))
    startDownload(archiveFilename(), url)
    // The browser takes its own reference to the blob shortly after the click,
    // not during it, so revoking immediately can cancel the download. Hold the
    // URL well past that point, then let the blob go.
    window.setTimeout(() => {
      URL.revokeObjectURL(url)
    }, DOWNLOAD_URL_LIFETIME_MS)
  } catch (e) {
    await modalAlert({
      title: 'Export failed',
      message: `Your files could not be exported.\n\n${e instanceof Error ? e.message : String(e)}`,
    })
  } finally {
    isArchiving = false
    startAutosaving()
  }
}

// Opens the file's saved history (issue #42). Browsing deliberately saves
// nothing: the editor's contents are shown as their own "current" row, so
// recording them here would only add an entry identical to it. Restoring is
// what pins state, since that is when there is something to lose.
async function handleHistory(target?: string) {
  if (!fileHistory) return
  if (!(await requireServer('Browsing file history'))) return
  const histories = await fileHistory.list()
  // A named file is the one whose menu this was opened from; otherwise the
  // toolbar button opens on whatever is being edited.
  const focus = target ?? currentFile.value
  // A file with nothing recorded yet still belongs in the picker: it is the
  // one the student is looking at.
  historyFiles.value =
    focus !== null && !histories.some((h) => h.filename === focus)
      ? [{ filename: focus }, ...histories]
      : histories
  if (historyFiles.value.length === 0) {
    await modalAlert({
      title: 'File history',
      message: 'No file has a saved history yet.',
    })
    return
  }
  // Deleting a file leaves nothing open, which is precisely when a student
  // reaches for the history -- so fall back to the first file that has one.
  await showHistoryOf(focus ?? historyFiles.value[0].filename)
  showHistory.value = true
}

/** Loads `filename`'s history into the modal. */
async function showHistoryOf(filename: string) {
  if (!fileHistory) return
  historyFile.value = filename
  historySelected.value = null
  let snapshots: SnapshotRef[]
  try {
    snapshots = (await fileHistory.index(filename)).snapshots
  } catch {
    snapshots = []
  }
  // The picker may have moved on while this read was in flight -- arrowing
  // through a <select> fires one change per key. The newest selection wins,
  // not the read that happens to finish last, or the modal would show one
  // file's versions under another file's name and restore them onto it.
  if (historyFile.value !== filename) return
  historySnapshots.value = snapshots
  // Only the open file has a "current" version to compare against; another
  // file's history is browsed on its own.
  historyContents.value =
    filename === currentFile.value && isEditorLoaded() ? editor().getDoc() : null
}

/**
 * Loads the contents of the version the browser just selected. Guarded the same
 * way `showHistoryOf` is: arrowing down the list fires one change per key, and
 * the newest pick has to win rather than whichever read lands last.
 */
async function handleSelectSnapshot(snapshot: SnapshotRef | null) {
  if (!fileHistory || snapshot === null) {
    historySelected.value = null
    return
  }
  const filename = historyFile.value
  historySelected.value = null
  let contents: string | null
  try {
    contents = await fileHistory.read(filename, snapshot.id)
  } catch {
    contents = null
  }
  if (historyFile.value !== filename) return
  historySelected.value = contents
}

function handleHistoryClose() {
  showHistory.value = false
  historyFiles.value = []
  historySnapshots.value = []
  historySelected.value = null
}

// Restores a snapshot by editing the document rather than reloading it, so the
// restore is undoable and keeps the cursor. Both the version being left and the
// one being restored are pinned in the history, so a restore never costs the
// student the state they were in.
async function handleRestoreSnapshot(snapshot: SnapshotRef) {
  if (!fs || !fileSession || !fileHistory) return
  // The modal is only opened while the server is reachable, but it stays open
  // across a drop -- so the click that restores has to check again.
  if (!(await requireServer('Restoring a saved version'))) return
  const filename = historyFile.value

  try {
    // Read before closing rather than trusting the preview to have loaded: the
    // button is live as soon as a version is picked, and a snapshot can age out
    // of a long history between listing it and restoring it.
    const contents = await fileHistory.read(filename, snapshot.id)
    handleHistoryClose()
    if (contents === null) {
      await modalAlert({
        title: 'Restore failed',
        message: 'That saved version is no longer available.',
      })
      return
    }

    if (filename !== currentFile.value) {
      // Recovering another file, possibly one that was deleted: write it back
      // and open it, so the student lands in what they recovered. Saving marks
      // its history as no longer deleted.
      //
      // Pin what the file holds first, exactly as the open-file branch below
      // does. A save inside the merge window reaches disk without becoming a
      // snapshot, so without this the overwrite could take those contents off
      // both disk and timeline. A deleted file has nothing to pin.
      if (await fs.fileExists(filename)) {
        await fileHistory.record(filename, await fs.loadFile(filename), new Date(), {
          force: true,
        })
      }
      await fs.saveFile(filename, contents)
      await switchToFile(filename)
      await fileSession.save({ force: true })
      return
    }

    if (!isEditorLoaded()) return
    await fileSession.save({ force: true })
    // That pin is what makes a restore safe to undo, and offline it does not
    // happen: `save` declines silently when there is nowhere to write (see
    // file-session.ts). Replacing the document anyway would trade the student's
    // unsaved work for an old version that cannot be written back either, so
    // check again in the gap the save just opened.
    if (!(await requireServer('Restoring a saved version'))) return
    editor().replaceDoc(contents)
    await fileSession.save({ force: true })
  } catch (e) {
    // Reaches here when the server goes away mid-restore, which reportError
    // turns into the dismissable offline notice rather than an error screen.
    reportError(e, (message) => `Could not restore that version: ${message}`)
  }
}

async function handleSelectFile(filename: string) {
  if (isLoadingFile) return
  // Switching forces a save of the outgoing file first (#238), which offline
  // cannot happen -- so the edit sitting in the editor would be dropped on the
  // way to a file that then fails to load. Refuse both halves at once.
  if (!(await requireServer('Opening another file'))) return
  await switchToFile(filename)
}

// ---------- page lifecycle handlers ----------

async function handleVisibilityChange() {
  if (document.visibilityState === 'hidden') {
    await saveCurrentFile()
    saveConfig()
  }
}

async function handlePageHide() {
  await saveCurrentFile()
  saveConfig()
  // The page may be frozen for the back/forward cache rather than destroyed,
  // so hand the lock back and re-acquire if we are restored. Pairing the two
  // is correct whether or not a frozen page keeps its lock.
  SingleInstance.releaseLock()
}

async function handlePageShow(e: PageTransitionEvent) {
  // Only a bfcache restore needs this; a fresh load acquires in onMounted.
  if (!e.persisted) return
  if (!(await SingleInstance.acquireLock())) {
    displayError(OTHER_INSTANCE_MESSAGE)
  }
}

async function handleBeforeUnload(e: BeforeUnloadEvent) {
  await saveCurrentFile()
  saveConfig()
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
const pageShowWrapper = (e: Event) => {
  void handlePageShow(e as PageTransitionEvent)
}
const beforeUnloadWrapper = (e: Event) => {
  void handleBeforeUnload(e as BeforeUnloadEvent)
}

// ---------- lifecycle ----------

onMounted(async () => {
  await FS.initialize()
  fs = FS.getFS()
  fileHistory = FS.getHistory()
  fileSession = new FileSession(
    fs,
    fileHistory,
    { getDoc: () => editor().getDoc(), isEditorLoaded },
    {
      onSaveError: (error) => {
        reportError(error, (message) => message)
      },
      // Offline, autosave declines rather than stopping: the timer keeps
      // ticking and starts writing again by itself once the server answers.
      canSave: () => !filesOnServer || connection.value === 'online',
    },
  )

  // A reconnect is the moment the editor's contents and the server's copy
  // diverge the most, so save at once rather than waiting for the next tick.
  stopWatchingConnection = Connectivity.onConnectionChange((state) => {
    if (state !== 'online') return
    offlineAnnounced = false
    void saveCurrentFile()
  })

  const obtainedLock = await SingleInstance.acquireLock()
  if (!obtainedLock) {
    loadingContent.value = OTHER_INSTANCE_MESSAGE
    return
  }

  // visibilitychange is a document event; pagehide/pageshow fire at the window
  // and never reach a document listener.
  document.addEventListener('visibilitychange', visibilityChangeWrapper)
  window.addEventListener('pagehide', pageHideWrapper)
  window.addEventListener('pageshow', pageShowWrapper)
  window.addEventListener('beforeunload', beforeUnloadWrapper)

  await loadConfig()

  // Everything that reaches for the file system is inside the try, because
  // startup is the one moment where an unreachable server used to be fatal:
  // these run before `isLoading` is cleared, so a rejection here left the
  // loading screen up forever with nothing said -- the exact dead end the
  // offline state exists to replace. The IDE opens instead, empty and honest.
  try {
    await populateFileDrawer()

    if (config.lastOpenedFilename !== null) {
      if (await fs.fileExists(config.lastOpenedFilename)) {
        // TODO: re-enable once we have a better handle on large-file loading
        // await switchToFile(config.lastOpenedFilename)
      } else {
        config.lastOpenedFilename = null
      }
    }
  } catch (e) {
    reportError(e, (message) => `Could not list your files: ${message}`)
    // An unreachable server is not a fault: fall through, open the IDE, and let
    // the heartbeat pick things up when it returns. Anything else is real, and
    // `displayError` has put it on the loading screen -- so stop here rather
    // than clearing that screen on the next line.
    if (!isUnreachable(e)) return
  }

  isLoading.value = false
  Scamper.getInstance().calibrateScheduler()

  showPatchNotesIfNeeded()
  await offerLocalFiles()
})

onUnmounted(() => {
  stopAutosaving()
  stopWatchingConnection?.()
  Connectivity.stop()
  session.stopAll()
  document.removeEventListener('visibilitychange', visibilityChangeWrapper)
  window.removeEventListener('pagehide', pageHideWrapper)
  window.removeEventListener('pageshow', pageShowWrapper)
  SingleInstance.releaseLock()
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
        :has-server="hasServer"
        :can-sign-in="canSignIn"
        :signed-in-as="signedInAs"
        :connection="connection"
        :sign-in="openSignIn"
        :sign-out="handleSignOut"
        :create="handleCreate"
        :rename="handleRename"
        :duplicate="handleDuplicate"
        :delete-file="handleDelete"
        :download="handleDownload"
        :archive="handleArchive"
        :history="handleHistory"
        :select-file="handleSelectFile"
        :upload="handlePickUpload"
        :file-drop="handleFileDrop"
      />
    </div>
    <div class="ide-main">
      <IdeMenuBar
        :current-file="currentFile"
        :has-server="hasServer"
        :can-sign-in="canSignIn"
        :signed-in-as="signedInAs"
        :is-sidebar-visible="isSidebarVisible"
        :recent-files="recentFiles"
        :create="handleCreate"
        :upload="handlePickUpload"
        :save="handleSave"
        :save-as="handleSaveAs"
        :close-file="handleCloseFile"
        :rename="handleRename"
        :delete-file="handleDelete"
        :download="handleDownload"
        :archive="handleArchive"
        :history="handleHistory"
        :select-file="handleSelectFile"
        :sign-in="openSignIn"
        :sign-out="handleSignOut"
        :run-window="handleRunWindow"
        :toggle-sidebar="toggleSidebar"
        :about="handleAbout"
        :whats-new="handleWhatsNew"
      />
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
  <SignInModal
    :open="showSignIn"
    :methods="signInMethods"
    :busy="signInBusy"
    :error="signInError"
    @password="handleSignInPassword"
    @close="closeSignIn"
  />
  <ModalHost />
  <PatchNotesModal
    :open="showPatchNotes"
    :notes="patchNotesToShow"
    @close="handlePatchNotesClose"
  />
  <FileHistoryModal
    v-if="showHistory"
    :open="showHistory"
    :files="historyFiles"
    :filename="historyFile"
    :snapshots="historySnapshots"
    :current-contents="historyContents"
    :selected-contents="historySelected"
    @close="handleHistoryClose"
    @select="showHistoryOf"
    @select-snapshot="handleSelectSnapshot"
    @restore="handleRestoreSnapshot"
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
