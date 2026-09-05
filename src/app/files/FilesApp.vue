<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import ThemeToggle from '../shared/ThemeToggle.vue'
import { archiveFilename } from '../web/archive'
import {
  describe,
  formatSize,
  formatWhen,
  sortEntries,
  totalSize,
} from './entries'
import {
  list,
  openRoot,
  readBlob,
  remove,
  rename,
  usage,
  zipAll,
  type StorageEntry,
  type StorageUsage,
} from './opfs-direct'

/*
 * The browser-storage rescue page (issue #130).
 *
 * A student who loads a file too large for Scamper to open has, until now, had
 * one way out: clear the site's data, which takes every other file with it.
 * This page is the smaller instrument -- it lists what is actually in storage,
 * hands any of it back as a download, and deletes what is in the way.
 *
 * Two deliberate limits keep it working when the IDE does not:
 *
 *  + It loads none of Scamper. No language, no editor, no `src/fs` backend;
 *    see the header of `opfs-direct.ts`.
 *  + Confirmations are `window.confirm`/`window.prompt` rather than the IDE's
 *    modal system. Fewer moving parts is the whole design goal here.
 *
 * It is also deliberately not linked from the IDE: it is reached by URL, so a
 * student cannot wander into it and delete their work by accident. See
 * `docs/browser-files.md`.
 */

const appVersion = APP_VERSION

/** The lock an open IDE tab holds; see `src/app/web/single-instance.ts`. */
const IDE_LOCK_NAME = 'scamper-single-instance'

/** The prefix under which the IDE keeps its settings in localStorage. */
const SETTINGS_PREFIX = 'scamper.'

/** What has to be typed to delete everything. */
const DELETE_ALL_PHRASE = 'delete everything'

// How long a generated object URL is kept alive after its download starts.
// Generous on purpose: the cost of holding a blob a few seconds too long is a
// little memory, while releasing it too early loses the download.
const DOWNLOAD_URL_LIFETIME_MS = 10_000

const entries = ref<StorageEntry[]>([])
const storageUsage = ref<StorageUsage | null>(null)
const error = ref<string | null>(null)
const notice = ref<string | null>(null)
const loaded = ref(false)
const busy = ref(false)
const ideOpenElsewhere = ref(false)

/** The OPFS root, opened once on load. */
let root: FileSystemDirectoryHandle | null = null

const summary = computed(() => {
  const count = entries.value.length
  const items = `${count} ${count === 1 ? 'item' : 'items'}, ${formatSize(totalSize(entries.value))} in all`
  const used = storageUsage.value
  if (used === null) return `${items}.`
  return `${items}. This site is using ${formatSize(used.used)} of the ${formatSize(used.quota)} your browser allows.`
})

function messageOf(e: unknown): string {
  return e instanceof Error ? e.message : String(e)
}

/** @returns the opened root, or throws the sentence the banner will show. */
function need(): FileSystemDirectoryHandle {
  if (root === null) {
    throw new Error('this browser’s storage could not be opened')
  }
  return root
}

/** Re-reads the listing and the storage estimate. */
async function refresh(): Promise<void> {
  root ??= await openRoot()
  entries.value = sortEntries(await list(root))
  // Before the estimate, not after: a browser that refuses one must not hide a
  // listing that worked, since the listing is what the student came for.
  loaded.value = true
  storageUsage.value = await usage()
}

/**
 * Runs `action`, reporting any failure in the page's banner.
 *
 * Nothing here may throw to a blank page: this is what a student reaches for
 * when everything else has already failed, so a fault has to arrive as a
 * sentence they can read.
 *
 * @param what what was being attempted, e.g. 'Deleting hello.scm'
 */
async function attempt(what: string, action: () => Promise<void>) {
  if (busy.value) return
  busy.value = true
  error.value = null
  notice.value = null
  try {
    await action()
  } catch (e) {
    error.value = `${what} failed: ${messageOf(e)}`
  } finally {
    busy.value = false
  }
}

/**
 * Notices an IDE tab that is already open.
 *
 * A query rather than a request, deliberately: holding the lock here would
 * leave the student unable to open the IDE afterwards, which is the opposite
 * of what this page is for.
 */
async function checkForOpenIde() {
  const locks = (navigator as { locks?: LockManager }).locks
  if (locks === undefined || typeof locks.query !== 'function') return
  try {
    const snapshot = await locks.query()
    ideOpenElsewhere.value = (snapshot.held ?? []).some(
      (lock) => lock.name === IDE_LOCK_NAME,
    )
  } catch {
    // Not being able to ask is not worth a banner.
  }
}

/** Hands `blob` to the browser as a download named `filename`. */
function startDownload(filename: string, blob: Blob) {
  const url = URL.createObjectURL(blob)
  const anchor = document.createElement('a')
  anchor.href = url
  anchor.download = filename
  anchor.click()
  // The browser takes its own reference to the blob shortly after the click,
  // not during it, so revoking immediately can cancel the download. Hold the
  // URL well past that point, then let the blob go.
  window.setTimeout(() => {
    URL.revokeObjectURL(url)
  }, DOWNLOAD_URL_LIFETIME_MS)
}

function handleDownload(entry: StorageEntry) {
  void attempt(`Downloading ${entry.name}`, async () => {
    startDownload(entry.name, await readBlob(need(), entry.name))
  })
}

function handleDownloadAll() {
  void attempt('Building the zip archive', async () => {
    startDownload(archiveFilename(), await zipAll(need()))
  })
}

function handleRename(entry: StorageEntry) {
  const to = window.prompt(`Rename ${entry.name} to:`, entry.name)?.trim()
  if (to === undefined || to === '' || to === entry.name) return
  // Renaming onto a name already in use replaces it. Every candidate name is
  // listed right above the prompt, so a mistyped one is easy, and this is the
  // page a student reaches when they are already close to losing work.
  if (
    entries.value.some((e) => e.name === to) &&
    !window.confirm(`${to} already exists. Replace it? This cannot be undone.`)
  ) {
    return
  }
  void attempt(`Renaming ${entry.name}`, async () => {
    await rename(need(), entry.name, to)
    await refresh()
    notice.value = `Renamed ${entry.name} to ${to}.`
  })
}

function handleDelete(entry: StorageEntry) {
  const what = entry.isDirectory
    ? `the folder ${entry.name} and everything in it`
    : entry.name
  if (!window.confirm(`Delete ${what}? This cannot be undone.`)) return
  void attempt(`Deleting ${entry.name}`, async () => {
    await remove(need(), entry.name)
    await refresh()
    notice.value = `Deleted ${entry.name}.`
  })
}

function handleDeleteAll() {
  const typed = window.prompt(
    `This deletes everything listed below and cannot be undone.\n\nType "${DELETE_ALL_PHRASE}" to confirm:`,
  )
  if (typed?.trim().toLowerCase() !== DELETE_ALL_PHRASE) return
  void attempt('Deleting everything', async () => {
    const dir = need()
    const failed: string[] = []
    for (const entry of [...entries.value]) {
      try {
        await remove(dir, entry.name)
      } catch {
        // Keep going: one stubborn entry must not strand the rest.
        failed.push(entry.name)
      }
    }
    await refresh()
    if (failed.length > 0) {
      throw new Error(`these could not be deleted: ${failed.join(', ')}`)
    }
    notice.value = 'Everything was deleted.'
  })
}

/**
 * Clears the IDE's settings, which live in localStorage rather than in
 * storage (`src/app/web/ide-config.ts`) -- so a file too large to open can
 * still be the file the IDE tries to reopen on every start.
 */
function handleResetSettings() {
  if (
    !window.confirm(
      'Reset Scamper’s settings? This forgets which file was open, your editor and run preferences, and the window layout. Your files are not touched.',
    )
  ) {
    return
  }
  error.value = null
  notice.value = null
  try {
    const keys: string[] = []
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i) ?? ''
      if (key.startsWith(SETTINGS_PREFIX)) keys.push(key)
    }
    for (const key of keys) localStorage.removeItem(key)
    notice.value = `Scamper’s settings were reset (${keys.length} cleared). Your files were not touched.`
  } catch (e) {
    error.value = `Resetting the settings failed: ${messageOf(e)}`
  }
}

onMounted(() => {
  void attempt('Listing your files', refresh)
  void checkForOpenIde()
})
</script>

<template>
  <div class="files-root">
    <div class="header">
      <div class="header-left">
        <a href="index.html">Scamper</a> <span>({{ appVersion }})</span> ⋅
        <span>Browser files</span>
      </div>
      <div class="header-right">
        <ThemeToggle />
        ⋅
        <a href="https://github.com/slag-plt/scamper"
          ><i class="fa-brands fa-github"></i
        ></a>
      </div>
    </div>

    <main class="content">
      <h1>Files stored in this browser</h1>
      <p class="intro">
        Everything Scamper keeps in this browser is listed below, including the
        files the editor hides from you. Use this page when Scamper will not
        start, or when a file is too large to open: download what you want to
        keep, then delete what is in the way. These files live in this browser
        only — nothing here is sent anywhere, and deleting one deletes it for
        good.
      </p>

      <p v-if="error" class="banner banner--error" role="alert">{{ error }}</p>
      <p v-if="notice" class="banner banner--notice" role="status">
        {{ notice }}
      </p>
      <p v-if="ideOpenElsewhere" class="banner banner--warn" role="status">
        Scamper is open in another tab — close it before deleting files, or it
        may write them back.
      </p>

      <p v-if="loaded" class="summary">{{ summary }}</p>

      <div class="actions">
        <button
          type="button"
          class="action"
          :disabled="busy || !loaded"
          @click="handleDownloadAll"
        >
          Download everything as a zip
        </button>
        <button
          type="button"
          class="action action--danger"
          :disabled="busy || entries.length === 0"
          @click="handleDeleteAll"
        >
          Delete all files
        </button>
        <button
          type="button"
          class="action"
          :disabled="busy"
          @click="handleResetSettings"
        >
          Reset Scamper’s settings
        </button>
      </div>
      <p class="note">
        Resetting the settings forgets which file was open, your editor and run
        preferences, and the window layout. It does not touch any of the files
        below, and it leaves your light/dark choice alone.
      </p>

      <table v-if="loaded && entries.length > 0" class="entries">
        <thead>
          <tr>
            <th scope="col">Name</th>
            <th scope="col" class="numeric">Size</th>
            <th scope="col">Last changed</th>
            <th scope="col">Actions</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="entry in entries" :key="entry.name">
            <td>
              <div class="name">{{ entry.name }}</div>
              <div class="description">{{ describe(entry) }}</div>
            </td>
            <td class="numeric">
              {{ entry.isDirectory ? '—' : formatSize(entry.size) }}
            </td>
            <td>{{ formatWhen(entry.lastModified) || '—' }}</td>
            <td>
              <div class="row-actions">
                <button
                  v-if="!entry.isDirectory"
                  type="button"
                  class="icon-button fa-solid fa-download"
                  :title="`Download ${entry.name}`"
                  :aria-label="`Download ${entry.name}`"
                  :disabled="busy"
                  @click="handleDownload(entry)"
                ></button>
                <button
                  v-if="!entry.isDirectory"
                  type="button"
                  class="icon-button fa-solid fa-pen"
                  :title="`Rename ${entry.name}`"
                  :aria-label="`Rename ${entry.name}`"
                  :disabled="busy"
                  @click="handleRename(entry)"
                ></button>
                <button
                  type="button"
                  class="icon-button fa-solid fa-trash"
                  :title="`Delete ${entry.name}`"
                  :aria-label="`Delete ${entry.name}`"
                  :disabled="busy"
                  @click="handleDelete(entry)"
                ></button>
              </div>
            </td>
          </tr>
        </tbody>
      </table>

      <p v-if="loaded && entries.length === 0" class="empty">
        There is nothing stored in this browser.
      </p>
    </main>
  </div>
</template>

<style>
html,
body,
#app {
  width: 100%;
  min-height: 100%;
  margin: 0;
  padding: 0;
  font-family: var(--font-sans);
  font-size: 1em;
}
</style>

<style scoped>
.files-root {
  min-height: 100%;
  display: flex;
  flex-direction: column;
}

.header {
  background: var(--header-bg);
  color: var(--header-fg);
  padding: var(--space-md);
  flex: 0 0 auto;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: space-between;
  gap: var(--space-md);
}

.header-left,
.header-right {
  color: var(--header-fg);
  display: flex;
  align-items: center;
  gap: var(--space-xs);
}

.content {
  flex: 1;
  width: 100%;
  max-width: 60rem;
  margin: 0 auto;
  padding: var(--space-2xl) var(--space-xl);
}

h1 {
  margin: 0 0 var(--space-md);
  font-size: var(--text-lg);
}

.intro,
.note {
  margin: 0 0 var(--space-xl);
  max-width: 46rem;
  line-height: var(--leading-normal);
  color: var(--fg);
}

.note {
  font-size: var(--text-sm);
  color: var(--syntax-comment);
}

.summary {
  margin: 0 0 var(--space-md);
  font-size: var(--text-md);
}

.banner {
  margin: 0 0 var(--space-md);
  padding: var(--space-md) var(--space-lg);
  border: 1px solid var(--test-border);
  border-radius: var(--radius-md);
  background: var(--surface);
  line-height: var(--leading-normal);
}

.banner--error {
  background: var(--test-error-bg);
}

.banner--notice {
  background: var(--test-ok-bg);
}

.banner--warn {
  background: var(--surface-muted);
}

.actions {
  display: flex;
  flex-wrap: wrap;
  gap: var(--space-md);
  margin-bottom: var(--space-md);
}

.action {
  padding: var(--space-xs) var(--space-md);
  font: inherit;
  font-size: var(--text-md);
  color: inherit;
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  cursor: pointer;
}

.action:hover:not(:disabled) {
  background: var(--surface-hover);
}

.action:disabled {
  opacity: 0.4;
  cursor: default;
}

.action--danger {
  color: var(--danger);
  border-color: var(--danger);
}

.entries {
  width: 100%;
  border-collapse: collapse;
  font-size: var(--text-md);
}

.entries th,
.entries td {
  padding: var(--space-md);
  text-align: left;
  border-bottom: 1px solid var(--border-muted);
  vertical-align: top;
}

.entries th {
  border-bottom: 1px solid var(--border);
}

.numeric {
  text-align: right;
  white-space: nowrap;
}

.name {
  font-family: var(--font-mono);
  overflow-wrap: anywhere;
}

.description {
  font-size: var(--text-xs);
  color: var(--syntax-comment);
}

.row-actions {
  display: flex;
  gap: var(--space-xs);
  justify-content: flex-end;
}

.empty {
  margin-top: var(--space-xl);
  color: var(--syntax-comment);
}
</style>
