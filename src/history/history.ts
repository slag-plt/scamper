/**
 * A file's save history (issue #42), as an interface with more than one
 * backing store behind it.
 *
 * The split that shapes this interface is metadata versus contents. Browsing a
 * history asks "which versions are there", and only then "what was in this
 * one". A flat file holding every snapshot answers both at once because it has
 * already read everything; a database answers the first without touching a
 * single byte of file contents. Keeping the two apart is what lets the
 * server-backed implementation avoid moving whole files to draw a list of
 * timestamps.
 */

/** Identifies one snapshot within a file's history. */
export interface SnapshotRef {
  /** Opaque, and only meaningful within one file's history. */
  id: string
  /** When the snapshot was taken -- never updated afterwards. ISO-8601. */
  time: string
}

/** A snapshot together with what the file held at the time. */
export interface Snapshot extends SnapshotRef {
  contents: string
}

/** A file that has a saved history, whether or not the file still exists. */
export interface HistoryFile {
  filename: string
  /** Set when the file itself was deleted; its history is still recoverable. */
  deletedAt?: string
}

/** One file's history without any contents: what the browser list needs. */
export interface HistoryIndex {
  /** Newest first. */
  snapshots: SnapshotRef[]
  /** Set when the file was deleted; its history outlives it. */
  deletedAt?: string
}

export interface RecordOptions {
  /** Takes a snapshot even inside the merge window, e.g. on close or switch. */
  force?: boolean
  /**
   * The caller's cached head, letting a save that changes nothing skip storage
   * entirely -- and, on a server-backed history, skip the request as well.
   * Sound because one instance holds the file lock (lockfile.ts). Leave it out
   * -- rather than passing null -- when the head isn't known; callers must drop
   * a cached head when the file is deleted or renamed.
   */
  knownHead?: Snapshot | null
}

export interface RecordResult {
  /** The newest snapshot after this call, for the caller to cache. */
  head: Snapshot | null
  /** True iff a new snapshot was added. */
  recorded: boolean
}

/** Somewhere a file's save history is kept. */
export interface History {
  /**
   * Lists every file with a saved history, deleted ones included, sorted by
   * name. Reads no contents.
   */
  list(): Promise<HistoryFile[]>

  /**
   * @returns `filename`'s snapshot times, newest first, and its deletion mark
   *          if it has one. Empty when there is no history. Reads no contents.
   */
  index(filename: string): Promise<HistoryIndex>

  /**
   * @returns what the file held at `id`, or null if that snapshot is gone
   */
  read(filename: string, id: string): Promise<string | null>

  /**
   * Records `contents` as `filename`'s newest snapshot, unless the save adds
   * nothing (see `addsNothing`). Drops the oldest entries past `MAX_SNAPSHOTS`
   * and clears any deletion mark, since the file is plainly back.
   * @param now the time to stamp the snapshot with. A server-backed history
   *        stamps with its own clock instead -- see `ServerHistory` -- so that
   *        snapshots from two machines stay comparable.
   */
  record(
    filename: string,
    contents: string,
    now: Date,
    options?: RecordOptions,
  ): Promise<RecordResult>

  /**
   * Moves `from`'s history to `to`, if it has one, overwriting whatever history
   * `to` already had -- including the retained history of a deleted file of
   * that name. Callers that can reach that case should confirm first.
   */
  rename(from: string, to: string): Promise<void>

  /**
   * Marks `filename`'s history as deleted, keeping it so the file can be
   * recovered afterwards (#42). A file with no history stays without one --
   * deleting shouldn't leave litter behind.
   */
  markDeleted(filename: string, now: Date): Promise<void>
}

const MONTHS = [
  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
]

/**
 * Renders a snapshot's time for the history list: `2:04pm` for one taken
 * today, `Aug 7, 2:04pm` earlier this year, `Aug 7 2025, 2:04pm` before that.
 * Histories of deleted files are kept indefinitely, so the year has to appear
 * or last year's version reads as this week's. Formatted by hand rather than
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
  const sameYear = at.getFullYear() === now.getFullYear()
  const sameDay =
    sameYear && at.getMonth() === now.getMonth() && at.getDate() === now.getDate()
  if (sameDay) return clock
  const day = `${MONTHS[at.getMonth()]} ${at.getDate().toString()}`
  return sameYear
    ? `${day}, ${clock}`
    : `${day} ${at.getFullYear().toString()}, ${clock}`
}
