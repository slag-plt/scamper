import type { FS } from '../../src/fs/fs'

/**
 * A file system holding nothing, for a spec that needs one only so the code
 * under test has something to call. Anything that would touch a file rejects.
 *
 * @param overrides the one or two methods the spec is actually about
 */
export function emptyFS(overrides: Partial<FS> = {}): FS {
  const nope = () => Promise.reject(new Error('unimplemented'))
  return {
    getFileList: () => Promise.resolve([]),
    fileExists: () => Promise.resolve(false),
    loadFile: nope,
    saveFile: nope,
    loadBytes: nope,
    saveBytes: nope,
    deleteFile: nope,
    renameFile: nope,
    ...overrides,
  }
}
