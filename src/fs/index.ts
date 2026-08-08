
import type { FS } from './fs'
import type { History } from '../history/history'
import { FlatFileHistory } from '../history/flat-file'
import OPFSFileSystem from './opfs'

export type { FS as t } from './fs'

/**
 * Where a user's files live, and where their save history lives with them.
 *
 * The two travel together rather than as separate globals because the wrong
 * pairing fails silently and badly: a server-backed file system with a
 * flat-file history would write `.{filename}.history` blobs into the server's
 * file storage, which is exactly the layout the server exists to replace. One
 * object makes that combination unrepresentable.
 */
export interface Backend {
  fs: FS
  history: History
}

let current: Backend | undefined = undefined

/**
 * @returns `fs` paired with the history layout that suits a file system whose
 *          files are just files -- OPFS, a directory on disk, a test double.
 *          Not for `ServerFileSystem`, which keeps history in its database
 *          rather than in `.{filename}.history` blobs.
 */
export function localBackend(fs: FS): Backend {
  return { fs, history: new FlatFileHistory(fs) }
}

/** Initializes the global backend to browser-local storage. */
export async function initialize(): Promise<void> {
  current ??= localBackend(await OPFSFileSystem.create())
}

/**
 * Installs `backend` as the global file system and history, replacing any
 * existing pair. Used by non-browser hosts (e.g. the CLI) to wire in something
 * other than the browser-only OPFS default, and by logging in and out
 * (issue #357): a logged-in user gets a `ServerFileSystem` alongside a
 * `ServerHistory`, and logging out puts OPFS and its flat files back.
 *
 * Swapping is safe at any point because every consumer -- the scheduler, scope
 * checking, the `file-exists?` primitive -- calls `getFS()` afresh rather than
 * holding onto an instance.
 */
export function setBackend(backend: Backend): void {
  current = backend
}

/**
 * @returns a handle to the global file system, assumes that it has already
 *          been successfully initialized
 */
export function getFS(): FS {
  if (!current) {
    throw new Error('File system not initialized')
  }
  return current.fs
}

/**
 * @returns the history that belongs to the current file system, assumes that
 *          it has already been successfully initialized
 */
export function getHistory(): History {
  if (!current) {
    throw new Error('File system not initialized')
  }
  return current.history
}
