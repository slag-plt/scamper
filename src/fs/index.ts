import * as F from './fs'
import OPFSFileSystem from './opfs'

let instance: F.FS | undefined = undefined

/** Initializes the global file system */
export async function initialize(): Promise<void> {
  instance ??= await OPFSFileSystem.create()
}

/**
 * @returns a handle to the global file system, assumes that it has already
 *          been successfully initialized
 */
export function FS(): F.FS {
  if (!instance) {
    throw new Error('File system not initialized')
  }
  return instance
}