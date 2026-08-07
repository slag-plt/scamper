
import type { FS } from './fs'
import OPFSFileSystem from './opfs'

export type { FS as t } from './fs'

let instance: FS | undefined = undefined

/** Initializes the global file system */
export async function initialize(): Promise<void> {
  instance ??= await OPFSFileSystem.create()
}

/**
 * Installs `fs` as the global file system, replacing any existing instance.
 * Used by non-browser hosts (e.g. the CLI) to wire in a file system other than
 * the browser-only OPFS default.
 *
 * This is also the seam for logging in and out (issue #357): a logged-in user
 * gets `setFS(ServerFileSystem.create(url))` from `./server`, where `url` comes
 * from `loadServerConfig()` in `./config`, and logging out puts OPFS back.
 * Swapping is safe at any point because every consumer -- the scheduler, scope
 * checking, the `file-exists?` primitive -- calls `getFS()` afresh rather than
 * holding onto an instance. The switch is deliberately driven by login state
 * and not by the mere presence of a config: a configured server only means one
 * is available to log in to.
 */
export function setFS(fs: FS): void {
  instance = fs
}

/**
 * @returns a handle to the global file system, assumes that it has already
 *          been successfully initialized
 */
export function getFS(): FS {
  if (!instance) {
    throw new Error('File system not initialized')
  }
  return instance
}