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
import type { HistoryStore, RecordOutcome } from './stores'

/**
 * Save history in memory, per user.
 *
 * Mirrors the row shape `MariaDbHistoryStore` uses -- histories keyed by
 * filename with a deletion mark, each holding snapshots newest-first -- so the
 * tests that drive the route layer against it are testing the same behaviour
 * the database gives. In particular, listing and indexing never touch contents,
 * because the queries they stand in for do not either.
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

export class MemoryHistoryStore implements HistoryStore {
  private readonly users = new Map<string, Map<string, StoredHistory>>()

  /** Stands in for the table's auto-increment column. */
  private nextId = 1

  list(userId: string): Promise<HistoryFile[]> {
    const files = [...this.historiesOf(userId).entries()]
      .filter(([, history]) => history.snapshots.length > 0)
      .map(([filename, history]) =>
        history.deletedAt === undefined
          ? { filename }
          : { filename, deletedAt: history.deletedAt },
      )

    return Promise.resolve(
      files.sort((a, b) => a.filename.localeCompare(b.filename)),
    )
  }

  index(userId: string, filename: string): Promise<HistoryIndex> {
    const history = this.historiesOf(userId).get(filename)
    if (history === undefined) return Promise.resolve({ snapshots: [] })

    const snapshots: SnapshotRef[] = history.snapshots.map((snapshot) => ({
      id: snapshot.id.toString(),
      time: snapshot.takenAt,
    }))

    return Promise.resolve(
      history.deletedAt === undefined
        ? { snapshots }
        : { snapshots, deletedAt: history.deletedAt },
    )
  }

  read(userId: string, filename: string, id: string): Promise<string | null> {
    const found = this.historiesOf(userId)
      .get(filename)
      ?.snapshots.find((snapshot) => snapshot.id.toString() === id)

    return Promise.resolve(found?.contents ?? null)
  }

  record(
    userId: string,
    filename: string,
    contents: string,
    now: Date,
    force: boolean,
  ): Promise<RecordOutcome> {
    // No history of the IDE's own state, matching the flat-file backend.
    if (isHiddenName(filename)) {
      return Promise.resolve({ recorded: false, head: null })
    }

    const histories = this.historiesOf(userId)
    const history = histories.get(filename) ?? { snapshots: [] }
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
        histories.set(filename, history)
      }
      return Promise.resolve({ recorded: false, head: headOf(history) })
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
    histories.set(filename, history)

    return Promise.resolve({ recorded: true, head: headOf(history) })
  }

  rename(userId: string, from: string, to: string): Promise<boolean> {
    if (isHiddenName(from) || isHiddenName(to)) return Promise.resolve(false)

    const histories = this.historiesOf(userId)
    const history = histories.get(from)
    if (history === undefined) return Promise.resolve(false)

    histories.delete(from)
    histories.set(to, history)
    return Promise.resolve(true)
  }

  markDeleted(userId: string, filename: string, now: Date): Promise<void> {
    const history = this.historiesOf(userId).get(filename)
    if (history !== undefined && history.snapshots.length > 0) {
      history.deletedAt = now.toISOString()
    }
    return Promise.resolve()
  }

  /** @returns `userId`'s histories, creating the namespace on first write. */
  private historiesOf(userId: string): Map<string, StoredHistory> {
    let histories = this.users.get(userId)
    if (histories === undefined) {
      histories = new Map<string, StoredHistory>()
      this.users.set(userId, histories)
    }
    return histories
  }
}

/** @returns the newest snapshot of `history`, with contents. */
function headOf(history: StoredHistory): Snapshot | null {
  if (history.snapshots.length === 0) return null
  const head = history.snapshots[0]
  return { id: head.id.toString(), time: head.takenAt, contents: head.contents }
}
