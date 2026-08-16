import { addsNothing } from './policy'
import { NotSignedInError } from '../fs/session'
import { fetchServer } from '../fs/unreachable'
import type {
  History,
  HistoryFile,
  HistoryIndex,
  RecordOptions,
  RecordResult,
  Snapshot,
  SnapshotRef,
} from './history'

/**
 * A history held by the Scamper server, one row per snapshot.
 *
 * Rows rather than a blob is the whole reason this exists separately. Listing
 * every file with a history is an indexed query that reads no contents at all;
 * recording is a single insert instead of rewriting fifty snapshots; drawing
 * the browser's list of times moves no file contents over the network.
 *
 * Two behaviours differ from the flat-file history, deliberately:
 *
 * - **The server stamps the time.** A history is now shared across a student's
 *   machines, and two clocks that disagree would interleave snapshots into an
 *   order that matches neither. The `now` this is handed is used only for the
 *   local skip check below.
 * - **The skip decision is made twice.** The caller's cached head settles the
 *   common case without a request at all -- autosave firing every few seconds
 *   on an unchanged document never reaches the network. When it cannot be
 *   ruled out locally, the server re-applies the same rule against what it
 *   actually holds, and its answer wins.
 */
export class ServerHistory implements History {
  /** API root, e.g. `https://host/api/v1`, without a trailing slash. */
  private readonly baseUrl: string

  private constructor(baseUrl: string) {
    this.baseUrl = baseUrl.replace(/\/+$/, '')
  }

  /**
   * @param baseUrl the API root the deployment advertises, as read from
   *                `src/fs/config.ts`
   */
  static create(baseUrl: string): ServerHistory {
    return new ServerHistory(baseUrl)
  }

  private async request(
    method: string,
    path: string,
    body?: unknown,
  ): Promise<Response> {
    const response = await fetchServer(`${this.baseUrl}/${path}`, {
      method,
      credentials: 'include',
      headers:
        body === undefined ? undefined : { 'Content-Type': 'application/json' },
      body: body === undefined ? undefined : JSON.stringify(body),
    })

    if (response.status === 401) {
      // Not a fault: the session lapsed, or was never there. The caller shows
      // the sign-in prompt rather than an error.
      throw new NotSignedInError()
    }
    if (!response.ok) {
      throw new Error(
        `File server ${method} ${path} failed: ${response.status.toString()} ${response.statusText}`,
      )
    }

    return response
  }

  async list(): Promise<HistoryFile[]> {
    const response = await this.request('GET', 'history/files')
    const body: unknown = await response.json()
    return readHistoryFiles(body)
  }

  async index(filename: string): Promise<HistoryIndex> {
    const response = await this.request('GET', `history/files/${encode(filename)}`)
    const body: unknown = await response.json()
    return readHistoryIndex(body)
  }

  async read(filename: string, id: string): Promise<string | null> {
    const response = await fetchServer(
      `${this.baseUrl}/history/files/${encode(filename)}/${encode(id)}`,
      { credentials: 'include' },
    )
    // A snapshot that has aged out from under the browser is an ordinary
    // outcome, not a failure: the list it was chosen from is a moment old.
    if (response.status === 401) throw new NotSignedInError()
    if (response.status === 404) return null
    if (!response.ok) {
      throw new Error(
        `File server GET history/files/${filename}/${id} failed: ${response.status.toString()}`,
      )
    }

    const body: unknown = await response.json()
    return typeof body === 'object' &&
      body !== null &&
      typeof (body as { contents: unknown }).contents === 'string'
      ? (body as { contents: string }).contents
      : null
  }

  async record(
    filename: string,
    contents: string,
    now: Date,
    options: RecordOptions = {},
  ): Promise<RecordResult> {
    const { force = false, knownHead } = options

    // Settled locally, so the steady state of autosave costs no requests.
    if (knownHead !== undefined && addsNothing(knownHead, contents, now, force)) {
      return { head: knownHead, recorded: false }
    }

    const response = await this.request('POST', `history/files/${encode(filename)}`, {
      contents,
      force,
    })
    const body: unknown = await response.json()
    return readRecordResult(body)
  }

  async rename(from: string, to: string): Promise<void> {
    await this.request('POST', 'history/rename', { from, to })
  }

  async markDeleted(filename: string): Promise<void> {
    await this.request('DELETE', `history/files/${encode(filename)}`)
  }
}

/** Escapes a value for use as a single path segment. */
function encode(value: string): string {
  return encodeURIComponent(value)
}

/** @returns the object at `key`'s array, or throws if the reply is malformed. */
function arrayField(body: unknown, key: string): unknown[] {
  const value =
    typeof body === 'object' && body !== null
      ? (body as Record<string, unknown>)[key]
      : undefined

  if (!Array.isArray(value)) {
    throw new Error(`File server returned a malformed ${key} list`)
  }

  return value
}

function readHistoryFiles(body: unknown): HistoryFile[] {
  return arrayField(body, 'files').map((entry) => {
    const { filename, deletedAt } = (entry ?? {}) as {
      filename: unknown
      deletedAt: unknown
    }

    if (typeof filename !== 'string') {
      throw new Error('File server returned a malformed history entry')
    }

    return typeof deletedAt === 'string'
      ? { filename, deletedAt }
      : { filename }
  })
}

function readSnapshotRef(entry: unknown): SnapshotRef {
  const { id, time } = (entry ?? {}) as { id: unknown; time: unknown }
  if (typeof id !== 'string' || typeof time !== 'string') {
    throw new Error('File server returned a malformed snapshot')
  }
  return { id, time }
}

function readHistoryIndex(body: unknown): HistoryIndex {
  const snapshots = arrayField(body, 'snapshots').map(readSnapshotRef)
  const deletedAt = (body as { deletedAt?: unknown }).deletedAt

  return typeof deletedAt === 'string'
    ? { snapshots, deletedAt }
    : { snapshots }
}

function readRecordResult(body: unknown): RecordResult {
  const { recorded, head } = (body ?? {}) as { recorded: unknown; head: unknown }
  if (typeof recorded !== 'boolean') {
    throw new Error('File server returned a malformed record result')
  }

  if (head === null || head === undefined) return { head: null, recorded }

  const ref = readSnapshotRef(head)
  const { contents } = head as { contents: unknown }
  if (typeof contents !== 'string') {
    throw new Error('File server returned a head without contents')
  }

  return { head: { ...ref, contents } satisfies Snapshot, recorded }
}

export default ServerHistory
