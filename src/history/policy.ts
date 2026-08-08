/**
 * When a save is worth recording. Shared, deliberately: the client applies this
 * to skip work it can rule out without asking, and the server applies the same
 * rule authoritatively against what it actually holds. One definition means the
 * two can never disagree about whether an edit deserved an entry.
 *
 * Pure by construction -- no DOM, no fetch, no file system -- which is what
 * makes it safe for the server to import as values.
 */

/** How many snapshots a file keeps before the oldest are dropped. */
export const MAX_SNAPSHOTS = 50

/**
 * How long a snapshot stays open to further edits. Saves within this long of
 * the newest entry's creation add nothing, so continuous editing yields about
 * one entry a minute rather than one every autosave.
 */
export const MERGE_WINDOW_MS = 60_000

/** The part of a snapshot this decision needs. */
export interface HeadState {
  /** When the head was created. ISO-8601. */
  time: string
  contents: string
}

/**
 * @param head the newest snapshot, or null if the file has no history yet
 * @param now the time the save is being recorded at
 * @param force records even inside the merge window, e.g. on close or switch
 * @returns true iff saving `contents` would add nothing to the history
 */
export function addsNothing(
  head: HeadState | null,
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
