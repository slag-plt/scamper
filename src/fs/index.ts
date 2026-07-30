
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