// Values from src/history/policy.ts, the second module (with src/fs/fs.ts)
// designated as shared. Applying the same predicate on both sides is the point:
// the client uses it to avoid asking, the server to decide authoritatively.
import { addsNothing, MAX_SNAPSHOTS } from '../../src/history/policy'
import { isHiddenName } from '../../src/fs/fs'
import type {
  HistoryFile,
  HistoryIndex,
  Snapshot,
  SnapshotRef,
} from '../../src/history/history'

/**
 * A stub history store: in memory, no authentication, one shared namespace.
 *
 * Its shape is the part meant to survive: `histories` keyed by filename with a
 * deletion mark, each holding rows of snapshots. That is the schema the MariaDB
 * implementation replaces it with (see server/schema.sql), which is why listing
 * and indexing here never touch contents -- the query they stand in for
 * wouldn't either.
 */

interface StoredSnapshot {
  id: number
  takenAt: string
  contents: string
}

interface StoredHistory {
  /** Newest first, mirroring the index the database reads them back through. */
  snapshots: StoredSnapshot[]
  deletedAt?: string
}

/** What a record attempt did, mirroring the client's `RecordResult`. */
export interface RecordOutcome {
  recorded: boolean
  head: Snapshot | null
}

export class HistoryStore {
  private readonly histories = new Map<string, StoredHistory>()

  /** Stands in for the table's auto-increment column. */
  private nextId = 1

  /** @returns every file with a history, deleted ones included, by name. */
  list(): HistoryFile[] {
    const files = [...this.histories.entries()]
      .filter(([, history]) => history.snapshots.length > 0)
      .map(([filename, history]) =>
        history.deletedAt === undefined
          ? { filename }
          : { filename, deletedAt: history.deletedAt },
      )

    return files.sort((a, b) => a.filename.localeCompare(b.filename))
  }

  /** @returns `filename`'s snapshot times, newest first. No contents. */
  index(filename: string): HistoryIndex {
    const history = this.histories.get(filename)
    if (history === undefined) return { snapshots: [] }

    const snapshots: SnapshotRef[] = history.snapshots.map((snapshot) => ({
      id: snapshot.id.toString(),
      time: snapshot.takenAt,
    }))

    return history.deletedAt === undefined
      ? { snapshots }
      : { snapshots, deletedAt: history.deletedAt }
  }

  /** @returns what `filename` held at `id`, or null if that row is gone. */
  read(filename: string, id: string): string | null {
    const found = this.histories
      .get(filename)
      ?.snapshots.find((snapshot) => snapshot.id.toString() === id)

    return found?.contents ?? null
  }

  /**
   * Records `contents` unless the save adds nothing, applying the same rule the
   * client does.
   * @param now the server's own clock. A history spans a student's machines, so
   *        two clocks that disagreed would interleave snapshots into an order
   *        matching neither -- the client's timestamp is deliberately not used.
   */
  record(
    filename: string,
    contents: string,
    now: Date,
    force: boolean,
  ): RecordOutcome {
    // No history of the IDE's own state, matching the flat-file backend.
    if (isHiddenName(filename)) return { recorded: false, head: null }

    const history = this.histories.get(filename) ?? { snapshots: [] }
    const head =
      history.snapshots.length === 0
        ? null
        : {
            time: history.snapshots[0].takenAt,
            contents: history.snapshots[0].contents,
          }

    if (addsNothing(head, contents, now, force)) {
      // A skipped save still clears a deletion mark: the file is plainly back,
      // and nothing else would clear it.
      if (history.deletedAt !== undefined) {
        delete history.deletedAt
        this.histories.set(filename, history)
      }
      return { recorded: false, head: headOf(history) }
    }

    const snapshot: StoredSnapshot = {
      id: this.nextId++,
      takenAt: now.toISOString(),
      contents,
    }
    // One insert plus a trim, rather than rewriting every snapshot -- the
    // reason a row-per-snapshot store exists.
    history.snapshots = [snapshot, ...history.snapshots].slice(0, MAX_SNAPSHOTS)
    delete history.deletedAt
    this.histories.set(filename, history)

    return { recorded: true, head: headOf(history) }
  }

  /**
   * Moves `from`'s history onto `to`, overwriting whatever `to` had.
   * @returns true iff `from` had a history to move
   */
  rename(from: string, to: string): boolean {
    if (isHiddenName(from) || isHiddenName(to)) return false

    const history = this.histories.get(from)
    if (history === undefined) return false

    this.histories.delete(from)
    this.histories.set(to, history)
    return true
  }

  /**
   * Marks `filename`'s history deleted, keeping it recoverable. A file with no
   * history stays without one -- deleting shouldn't leave litter behind.
   */
  markDeleted(filename: string, now: Date): void {
    const history = this.histories.get(filename)
    if (history === undefined || history.snapshots.length === 0) return

    history.deletedAt = now.toISOString()
  }
}

/** @returns the newest snapshot of `history`, with contents. */
function headOf(history: StoredHistory): Snapshot | null {
  if (history.snapshots.length === 0) return null
  const head = history.snapshots[0]
  return { id: head.id.toString(), time: head.takenAt, contents: head.contents }
}
