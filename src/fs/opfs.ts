import { isHiddenName, type FS, type FileEntry } from './fs'

/** The suffix Chromium gives the swap file backing an open writable. */
const SWAP_SUFFIX = '.crswap'

/** @returns true iff `get` resolves, i.e. an entry of that kind is present. */
async function entryExists(get: () => Promise<unknown>): Promise<boolean> {
  try {
    await get()
    return true
  } catch {
    return false
  }
}

/**
 * A wrapper around the OPFS API that simplifies access to files with a
 * higher-level API.
 */
export class OPFSFileSystem implements FS {
  private root?: FileSystemDirectoryHandle

  // eslint-disable-next-line @typescript-eslint/no-empty-function
  private constructor() { }

  /** @returns a new file system instance for accessing the OPFS */
  static async create(): Promise<OPFSFileSystem> {
    const ret = new OPFSFileSystem()
    ret.root = await navigator.storage.getDirectory()
    return ret
  }

  /** @return the list of files found at the root of the file system */
  async getFileList(): Promise<FileEntry[]> {
    const fileEntries: FileEntry[] = []

    // N.B., this.root doesn't have an entries field according to the type
    // checker... but it does! Declaring its shape here keeps the loop below
    // typed rather than spreading `any` through it.
    // https://developer.mozilla.org/en-US/docs/Web/API/FileSystemDirectoryHandle/entries
    const root = this.root as unknown as {
      entries: () => AsyncIterable<[string, FileSystemHandle]>
    }

    for await (const [name, handle] of root.entries()) {
      // Chromium implements createWritable() with a sibling `<name>.crswap`
      // file, which a listing sees while a write is in flight -- and which a
      // tab that died mid-save leaves behind for good. It is an artifact of
      // this backend, not a file anything above should ever be shown: without
      // this, one turns up in the file drawer and in an export of "all your
      // files", where reading it can also fail as it vanishes underneath.
      if (name.endsWith(SWAP_SUFFIX)) continue

      const isDirectory = handle.kind === 'directory'
      let preview: string | null = null

      // A preview costs a full read of the file, and only the file drawer
      // displays one -- which never shows dotted names. Skipping them keeps
      // internal files (the config, the lock, per-file histories) off the
      // listing's cost.
      if (!isDirectory && !isHiddenName(name)) {
        try {
          preview = await this.getFilePreview(handle as FileSystemFileHandle)
        } catch {
          preview = null
        }
      }

      fileEntries.push({ name, preview, isDirectory })
    }

    return fileEntries.sort((a, b) => {
      if (a.isDirectory && !b.isDirectory) return -1
      if (!a.isDirectory && b.isDirectory) return 1
      return a.name.localeCompare(b.name)
    })
  }

  /** @return a preview (prefix) of the file denoted by the given handle */
  private async getFilePreview(fileHandle: FileSystemFileHandle): Promise<string> {
    try {
      const file = await fileHandle.getFile()
      const text = await file.text()
      return text.split('\n').slice(0, 5).join('\n')
    } catch (e) {
      throw new Error(`Failed to get file preview: ${e}`)
    }
  }

  /** @return true iff the given file exists */
  async fileExists(filename: string): Promise<boolean> {
    // N.B., a direct lookup rather than a scan of getFileList(), which reads
    // every file in the root to build its previews. This runs on hot paths --
    // module resolution, import steps, and the `file-exists?` primitive a
    // student can call in a loop -- so it has to stay O(1) in the file count.
    //
    // A directory counts as existing: `file-exists?` is documented to say #t
    // for one, so a missed file lookup falls back to a directory lookup.
    return (
      (await entryExists(() => this.root!.getFileHandle(filename))) ||
      (await entryExists(() => this.root!.getDirectoryHandle(filename)))
    )
  }


  /** @return the contents of the given file, assumed to exist */
  async loadFile (filename: string): Promise<string> {
    const handle = await this.root!.getFileHandle(filename)
    const file = await handle.getFile()
    return await file.text()
  }

  /** Saves `contents` to the given file, creating it if it doesn't already exist */
  async saveFile (filename: string, contents: string): Promise<void> {
    const handle = await this.root!.getFileHandle(filename, { create: true })
    const stream = await handle.createWritable()
    await stream.write(contents)
    await stream.close()
  }

  async deleteFile (filename: string): Promise<void> {
    await this.root!.removeEntry(filename)
  }

  /** Renames the `from` file to the `to`. */
  async renameFile (from: string, to: string): Promise<void> {
    const contents = await this.loadFile(from)
    if (await this.fileExists(to)) {
      await this.deleteFile(to)
    }
    await this.saveFile(to, contents)
    await this.deleteFile(from)
  }
}

export default OPFSFileSystem