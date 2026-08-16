import { isHiddenName } from '../fs/fs'
import type * as FS from '../fs'
import { addsNothing, MAX_SNAPSHOTS } from './policy'
import type {
  History,
  HistoryFile,
  HistoryIndex,
  RecordOptions,
  RecordResult,
  Snapshot,
} from './history'

/**
 * A history kept beside each file as `.{filename}.history`, so it works on any
 * backing file system that stores bytes under a name -- OPFS in the browser,
 * a directory on the CLI.
 *
 * Snapshots hold whole contents rather than diffs: a student's program is a
 * couple of KB, a torn write costs one entry instead of every entry older than
 * it, and reconstructing a version is a lookup. `version` is what lets that
 * change later without a rewrite.
 *
 * The format is unchanged from when history was flat-file-only, so histories
 * written by earlier builds keep working.
 */

const HISTORY_VERSION = 1

/** One recorded save, as it is stored on disk. Ids are derived, not stored. */
interface StoredSnapshot {
  time: string
  contents: string
}

interface StoredHistory {
  version: typeof HISTORY_VERSION
  snapshots: StoredSnapshot[]
  deletedAt?: string
}

/**
 * @returns the name of the file holding `filename`'s history. Exported for
 *          this implementation's own tests; the layout is not part of the
 *          `History` interface and `src/history/index.ts` does not re-export it.
 */
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

/**
 * Derives a stable id per snapshot.
 *
 * The stored format has no ids, and an index cannot serve as one: a snapshot
 * recorded between listing a history and reading from it would shift every
 * index by one and quietly hand back the wrong version. A time is stable under
 * that insert. Exact duplicates -- possible only via two forced saves inside
 * the same millisecond -- are disambiguated by their order in the file.
 */
function idsFor(snapshots: StoredSnapshot[]): string[] {
  const seen = new Map<string, number>()
  return snapshots.map((snapshot) => {
    const n = seen.get(snapshot.time) ?? 0
    seen.set(snapshot.time, n + 1)
    return n === 0 ? snapshot.time : `${snapshot.time}#${n.toString()}`
  })
}

function isStoredSnapshot(value: unknown): value is StoredSnapshot {
  const s = value as Record<string, unknown> | null
  return (
    typeof s === 'object' &&
    s !== null &&
    typeof s.time === 'string' &&
    typeof s.contents === 'string'
  )
}

function isStoredHistory(value: unknown): value is StoredHistory {
  const h = value as Record<string, unknown> | null
  return (
    typeof h === 'object' &&
    h !== null &&
    h.version === HISTORY_VERSION &&
    Array.isArray(h.snapshots)
  )
}

export class FlatFileHistory implements History {
  private readonly fs: FS.t

  constructor(fs: FS.t) {
    this.fs = fs
  }

  /**
   * Reads `filename`'s stored history.
   * @returns the stored history, or an empty one if there is none, it cannot be
   *          parsed, or it was written by a version this build doesn't know. A
   *          history is a convenience, so a damaged one starts over rather than
   *          disabling snapshots for that file forever.
   */
  private async load(filename: string): Promise<StoredHistory> {
    const empty: StoredHistory = { version: HISTORY_VERSION, snapshots: [] }
    const target = historyFilename(filename)
    if (!(await this.fs.fileExists(target))) return empty

    try {
      const stored: unknown = JSON.parse(await this.fs.loadFile(target))
      if (!isStoredHistory(stored)) return empty
      return {
        version: HISTORY_VERSION,
        snapshots: stored.snapshots.filter(isStoredSnapshot),
        ...(typeof stored.deletedAt === 'string'
          ? { deletedAt: stored.deletedAt }
          : {}),
      }
    } catch {
      return empty
    }
  }

  private async write(filename: string, history: StoredHistory): Promise<void> {
    await this.fs.saveFile(historyFilename(filename), JSON.stringify(history))
  }

  /**
   * Unavoidably reads every history in full, since a flat file keeps its
   * deletion mark in the same blob as its snapshots. That cost is why this
   * belongs behind an explicit "show me the history" action rather than on a
   * hot path -- and why the server-backed history answers it from an index
   * instead.
   */
  async list(): Promise<HistoryFile[]> {
    const histories: HistoryFile[] = []

    for (const entry of await this.fs.getFileList()) {
      if (entry.isDirectory) continue
      const filename = fileOfHistory(entry.name)
      if (filename === null) continue

      const { deletedAt } = await this.load(filename)
      histories.push(
        deletedAt === undefined ? { filename } : { filename, deletedAt },
      )
    }

    return histories.sort((a, b) => a.filename.localeCompare(b.filename))
  }

  async index(filename: string): Promise<HistoryIndex> {
    const history = await this.load(filename)
    const ids = idsFor(history.snapshots)

    return {
      snapshots: history.snapshots.map((snapshot, i) => ({
        id: ids[i],
        time: snapshot.time,
      })),
      ...(history.deletedAt === undefined ? {} : { deletedAt: history.deletedAt }),
    }
  }

  async read(filename: string, id: string): Promise<string | null> {
    const history = await this.load(filename)
    const at = idsFor(history.snapshots).indexOf(id)
    return at < 0 ? null : history.snapshots[at].contents
  }

  async record(
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

    const history = await this.load(filename)
    const stored = history.snapshots[0] ?? null
    const skip = addsNothing(stored, contents, now, force)

    // A skipped save still has to write when the file was previously deleted:
    // the deletion mark is now wrong, and nothing else would clear it.
    if (skip && history.deletedAt === undefined) {
      return { head: headOf(history.snapshots), recorded: false }
    }

    const snapshot: StoredSnapshot = { time: now.toISOString(), contents }
    const snapshots = skip
      ? history.snapshots
      : [snapshot, ...history.snapshots].slice(0, MAX_SNAPSHOTS)
    await this.write(filename, { version: HISTORY_VERSION, snapshots })

    return { head: headOf(snapshots), recorded: !skip }
  }

  async rename(from: string, to: string): Promise<void> {
    if (isHiddenName(from) || isHiddenName(to)) return

    const source = historyFilename(from)
    if (await this.fs.fileExists(source)) {
      await this.fs.renameFile(source, historyFilename(to))
    }
  }

  async markDeleted(filename: string, now: Date): Promise<void> {
    if (isHiddenName(filename)) return

    const history = await this.load(filename)
    if (history.snapshots.length === 0) return

    await this.write(filename, { ...history, deletedAt: now.toISOString() })
  }
}

/** @returns the newest snapshot of `snapshots`, with its derived id. */
function headOf(snapshots: StoredSnapshot[]): Snapshot | null {
  if (snapshots.length === 0) return null
  const head = snapshots[0]
  return { id: idsFor(snapshots)[0], time: head.time, contents: head.contents }
}

export default FlatFileHistory
