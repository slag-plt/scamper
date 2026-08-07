import { isHiddenName } from '../../fs/fs'
import type * as FS from '../../fs'

/**
 * A file's save history, kept beside it as `.{filename}.history` so it works on
 * any backing file system (issue #42). Snapshots hold whole contents rather
 * than diffs: a student's program is a couple of KB, a torn write costs one
 * entry instead of every entry older than it, and reconstructing a version is
 * a lookup. `version` is what lets that change later without a rewrite.
 *
 * This module is pure over an injected `FS.t` -- no Vue, no DOM -- so it is
 * unit-testable on its own. `FileSession` owns when it is called.
 */

/** One recorded save of a file. */
export interface Snapshot {
  /** When the entry was created -- never updated afterwards. ISO-8601. */
  time: string
  contents: string
}

/** The recorded history of a single file, newest snapshot first. */
export interface FileHistory {
  version: typeof HISTORY_VERSION
  snapshots: Snapshot[]
  /** When the file was deleted, if it has been. Its history outlives it. */
  deletedAt?: string
}

export const HISTORY_VERSION = 1

/** How many snapshots a file keeps before the oldest are dropped. */
export const MAX_SNAPSHOTS = 50

/**
 * How long a snapshot stays open to further edits. Saves within this long of
 * the newest entry's creation add nothing, so continuous editing yields about
 * one entry a minute rather than one every autosave.
 */
export const MERGE_WINDOW_MS = 60_000

const MONTHS = [
  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
]

/**
 * Renders a snapshot's time for the history list: `2:04pm` for one taken
 * today, `Aug 7, 2:04pm` for an older one. Formatted by hand rather than
 * through toLocaleString so the result doesn't shift with the host's locale.
 * @param now the day to read `time` as relative to
 */
export function formatSnapshotTime(time: string, now: Date): string {
  const at = new Date(time)
  if (Number.isNaN(at.getTime())) return 'unknown'
  const hour = at.getHours() % 12 === 0 ? 12 : at.getHours() % 12
  const clock = `${hour.toString()}:${at.getMinutes().toString().padStart(2, '0')}${
    at.getHours() < 12 ? 'am' : 'pm'
  }`
  const sameDay =
    at.getFullYear() === now.getFullYear() &&
    at.getMonth() === now.getMonth() &&
    at.getDate() === now.getDate()
  return sameDay
    ? clock
    : `${MONTHS[at.getMonth()]} ${at.getDate().toString()}, ${clock}`
}

/** @returns the name of the file holding `filename`'s history. */
export function historyFilename(filename: string): string {
  return `.${filename}.history`
}

/** @returns the file a history file belongs to, or null if it isn't one. */
export function fileOfHistory(historyName: string): string | null {
  const suffix = '.history'
  if (!historyName.startsWith('.') || !historyName.endsWith(suffix)) return null
  const filename = historyName.slice(1, -suffix.length)
  return filename.length > 0 ? filename : null
}

/** A file that has a saved history, whether or not the file still exists. */
export interface HistoryFile {
  filename: string
  /** Set when the file itself was deleted; its history is still recoverable. */
  deletedAt?: string
}

/**
 * Lists every file with a saved history, deleted ones included, sorted by name.
 * Reads each history to find out whether its file is gone, so this belongs
 * behind an explicit "show me the history" action rather than on a hot path.
 */
export async function listHistories(fs: FS.t): Promise<HistoryFile[]> {
  const histories: HistoryFile[] = []
  for (const entry of await fs.getFileList()) {
    if (entry.isDirectory) continue
    const filename = fileOfHistory(entry.name)
    if (filename === null) continue
    const { deletedAt } = await loadHistory(fs, filename)
    histories.push(deletedAt === undefined ? { filename } : { filename, deletedAt })
  }
  return histories.sort((a, b) => a.filename.localeCompare(b.filename))
}

/**
 * Reads `filename`'s history.
 * @returns the stored history, or an empty one if there is none, it cannot be
 *          parsed, or it was written by a version this build doesn't know. A
 *          history is a convenience, so a damaged one starts over rather than
 *          disabling snapshots for that file forever.
 */
export async function loadHistory(
  fs: FS.t,
  filename: string,
): Promise<FileHistory> {
  const empty: FileHistory = { version: HISTORY_VERSION, snapshots: [] }
  const target = historyFilename(filename)
  if (!(await fs.fileExists(target))) return empty
  try {
    const stored: unknown = JSON.parse(await fs.loadFile(target))
    if (!isFileHistory(stored)) return empty
    return {
      version: HISTORY_VERSION,
      snapshots: stored.snapshots.filter(isSnapshot),
      ...(typeof stored.deletedAt === 'string'
        ? { deletedAt: stored.deletedAt }
        : {}),
    }
  } catch {
    return empty
  }
}

export interface RecordOptions {
  /** Takes a snapshot even inside the merge window, e.g. on close or switch. */
  force?: boolean
  /**
   * The caller's cached head, letting a save that changes nothing skip storage
   * entirely. Sound because one instance holds the file lock (lockfile.ts).
   * Leave it out -- rather than passing null -- when the head isn't known;
   * callers must drop a cached head when the file is deleted or renamed.
   */
  knownHead?: Snapshot | null
}

export interface RecordResult {
  /** The newest snapshot after this call, for the caller to cache. */
  head: Snapshot | null
  /** True iff a new snapshot was added. */
  recorded: boolean
}

/**
 * Records `contents` as `filename`'s newest snapshot, unless the save adds
 * nothing (see {@link addsNothing}). Dropping the oldest entries past
 * {@link MAX_SNAPSHOTS}, and clearing any deletion mark, since the file is
 * plainly back.
 * @param now the time to stamp the snapshot with
 */
export async function recordSnapshot(
  fs: FS.t,
  filename: string,
  contents: string,
  now: Date,
  options: RecordOptions = {},
): Promise<RecordResult> {
  // No history of the files that hold history, nor of the IDE's own state.
  if (isHiddenName(filename)) return { head: null, recorded: false }

  const { force = false, knownHead } = options
  if (knownHead !== undefined && addsNothing(knownHead, contents, now, force)) {
    return { head: knownHead, recorded: false }
  }

  const history = await loadHistory(fs, filename)
  const head = history.snapshots[0] ?? null
  const skip = addsNothing(head, contents, now, force)
  // A skipped save still has to write when the file was previously deleted:
  // the deletion mark is now wrong, and nothing else would clear it.
  if (skip && history.deletedAt === undefined) return { head, recorded: false }

  const snapshot: Snapshot = { time: now.toISOString(), contents }
  const snapshots = skip
    ? history.snapshots
    : [snapshot, ...history.snapshots].slice(0, MAX_SNAPSHOTS)
  await writeHistory(fs, filename, { version: HISTORY_VERSION, snapshots })
  return { head: skip ? head : snapshot, recorded: !skip }
}

/** Moves `from`'s history to `to`, if it has one. */
export async function renameHistory(
  fs: FS.t,
  from: string,
  to: string,
): Promise<void> {
  if (isHiddenName(from) || isHiddenName(to)) return
  const source = historyFilename(from)
  if (await fs.fileExists(source)) {
    await fs.renameFile(source, historyFilename(to))
  }
}

/**
 * Marks `filename`'s history as deleted, keeping it so the file can be
 * recovered afterwards (#42). A file with no history stays without one --
 * deleting shouldn't leave litter behind.
 */
export async function markHistoryDeleted(
  fs: FS.t,
  filename: string,
  now: Date,
): Promise<void> {
  if (isHiddenName(filename)) return
  const history = await loadHistory(fs, filename)
  if (history.snapshots.length === 0) return
  await writeHistory(fs, filename, {
    ...history,
    deletedAt: now.toISOString(),
  })
}

/**
 * @returns true iff saving `contents` would add nothing to a history headed by
 *          `head`.
 */
function addsNothing(
  head: Snapshot | null,
  contents: string,
  now: Date,
  force: boolean,
): boolean {
  if (head === null) return false
  // Autosave writes every few seconds whether or not the document changed (see
  // file-session.ts), so identical contents must never become an entry.
  if (head.contents === contents) return true
  // Measured from when the head was *created*, deliberately: a window that slid
  // forward on each edit would stay open for as long as the student kept
  // typing, and the history would never gain a second entry.
  return !force && now.getTime() - Date.parse(head.time) < MERGE_WINDOW_MS
}

async function writeHistory(
  fs: FS.t,
  filename: string,
  history: FileHistory,
): Promise<void> {
  await fs.saveFile(historyFilename(filename), JSON.stringify(history))
}

function isSnapshot(value: unknown): value is Snapshot {
  const s = value as Record<string, unknown> | null
  return (
    typeof s === 'object' &&
    s !== null &&
    typeof s.time === 'string' &&
    typeof s.contents === 'string'
  )
}

function isFileHistory(value: unknown): value is FileHistory {
  const h = value as Record<string, unknown> | null
  return (
    typeof h === 'object' &&
    h !== null &&
    h.version === HISTORY_VERSION &&
    Array.isArray(h.snapshots)
  )
}
