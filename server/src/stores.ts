// What the route layer stores things in. Two implementations: `store.ts` and
// `history-store.ts` keep everything in memory, for tests and for a checkout
// with no database; `mariadb-stores.ts` is the durable one.
//
// Every method takes the id of the user whose files these are. Scoping is a
// parameter rather than, say, a store built per request because it is the one
// rule that must never be forgotten: a query that omits it returns somebody
// else's work. Passing it explicitly means the type checker asks for it.

import { isHiddenName, type FileEntry } from '../../src/fs/fs'
import type {
  HistoryFile,
  HistoryIndex,
  Snapshot,
} from '../../src/history/history'

/** How many leading lines of a file the listing carries as its preview. */
export const PREVIEW_LINES = 5

/**
 * @returns the preview the file drawer shows for `name`, or null for a file it
 *          never displays.
 *
 * Computed server-side, which is the point of having a server: a listing costs
 * one request instead of one read per file. Dotted names carry no preview,
 * matching src/fs/opfs.ts and src/fs/node.ts -- not cosmetic, since a file's
 * saved history can hold fifty whole versions and previewing one would put
 * every past version of every file into a listing that displays none of them.
 */
export function previewOf(name: string, contents: string): string | null {
  return isHiddenName(name)
    ? null
    : contents.split('\n').slice(0, PREVIEW_LINES).join('\n')
}

/** What a record attempt did, mirroring the client's `RecordResult`. */
export interface RecordOutcome {
  recorded: boolean
  head: Snapshot | null
}

/** A user's files, one row per file. Mirrors the `FS` interface in src/fs/fs.ts. */
export interface FileStore {
  /**
   * @returns every file of `userId`'s, ordered the way the file drawer expects:
   *          by name, matching `src/fs/opfs.ts` so the two backends agree.
   */
  list(userId: string): Promise<FileEntry[]>

  /** @returns the contents of `name`, or undefined if it does not exist */
  read(userId: string, name: string): Promise<string | undefined>

  /** Saves `contents` to `name`, creating it if it does not already exist. */
  write(userId: string, name: string, contents: string): Promise<void>

  /** @returns true iff `name` existed and was removed */
  remove(userId: string, name: string): Promise<boolean>

  /**
   * Renames `from` to `to`, overwriting `to` if it exists.
   * @returns true iff `from` existed
   */
  rename(userId: string, from: string, to: string): Promise<boolean>
}

/** A user's save history: one row per recorded version (issue #42). */
export interface HistoryStore {
  /** @returns every file with a history, deleted ones included, by name. */
  list(userId: string): Promise<HistoryFile[]>

  /** @returns `filename`'s snapshot times, newest first. No contents. */
  index(userId: string, filename: string): Promise<HistoryIndex>

  /** @returns what `filename` held at `id`, or null if that row is gone. */
  read(userId: string, filename: string, id: string): Promise<string | null>

  /**
   * Records `contents` unless the save adds nothing, applying the same rule the
   * client does (src/history/policy.ts).
   * @param now the server's own clock. A history spans a student's machines, so
   *        two clocks that disagreed would interleave snapshots into an order
   *        matching neither -- the client's timestamp is deliberately not used.
   */
  record(
    userId: string,
    filename: string,
    contents: string,
    now: Date,
    force: boolean,
  ): Promise<RecordOutcome>

  /**
   * Moves `from`'s history onto `to`, overwriting whatever `to` had.
   * @returns true iff `from` had a history to move
   */
  rename(userId: string, from: string, to: string): Promise<boolean>

  /**
   * Marks `filename`'s history deleted, keeping it recoverable. A file with no
   * history stays without one -- deleting shouldn't leave litter behind.
   */
  markDeleted(userId: string, filename: string, now: Date): Promise<void>
}

/** The stores a request is dispatched against. */
export interface Stores {
  files: FileStore
  history: HistoryStore

  /**
   * Whether the storage behind these is answering, for the health route.
   *
   * Absent for the in-memory stores, which cannot be out of reach: they are
   * this process. Present for MariaDB, where the server can be perfectly well
   * while the database it needs is not -- a state that otherwise looks exactly
   * like "nobody is signed in", because reading a session is itself a query.
   */
  reachable?: () => Promise<boolean>
}
