import { isBinaryName, isHiddenName, refuseBinary, type Bytes, type FS, type FileEntry } from './fs'

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

  private constructor() {
    /* `create` is the only way in, and it is what sets `root` */
  }

  /**
   * The OPFS root.
   *
   * `root` is optional only because the constructor cannot await, so `create`
   * fills it in immediately afterwards and it is set for the whole of an
   * instance's life. Every method reached for it with a `!`; this says the
   * invariant once instead of eighteen times, and fails with a sentence rather
   * than a TypeError if it is ever broken.
   */
  private get dir(): FileSystemDirectoryHandle {
    if (this.root === undefined) {
      throw new Error('the OPFS file system was used before create() finished')
    }
    return this.root
  }

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

      // A preview costs a full read of the file. Internal files are not worth
      // that: a per-file history holds every saved version of its file, so
      // previewing one would drag the lot into a listing that shows a line of
      // it at most. (The drawer can now show dotted names -- see #178 -- it
      // just does not preview them.)
      if (!isDirectory && !isHiddenName(name) && !isBinaryName(name)) {
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
      (await entryExists(() => this.dir.getFileHandle(filename))) ||
      (await entryExists(() => this.dir.getDirectoryHandle(filename)))
    )
  }


  /** @return the contents of the given file, assumed to exist */
  async loadFile (filename: string): Promise<string> {
    refuseBinary(filename)
    const handle = await this.dir.getFileHandle(filename)
    const file = await handle.getFile()
    return await file.text()
  }

  /** Saves `contents` to the given file, creating it if it doesn't already exist */
  async saveFile (filename: string, contents: string): Promise<void> {
    refuseBinary(filename)
    const handle = await this.dir.getFileHandle(filename, { create: true })
    const stream = await handle.createWritable()
    await stream.write(contents)
    await stream.close()
  }

  /** @return the bytes of the given file, assumed to exist */
  async loadBytes (filename: string): Promise<Bytes> {
    const handle = await this.dir.getFileHandle(filename)
    const file = await handle.getFile()
    return new Uint8Array(await file.arrayBuffer())
  }

  /** Saves `bytes` to the given file, creating it if it doesn't already exist */
  async saveBytes (filename: string, bytes: Bytes): Promise<void> {
    const handle = await this.dir.getFileHandle(filename, { create: true })
    const stream = await handle.createWritable()
    // Wrapped in a Blob because the stream only accepts a view over a
    // non-shared ArrayBuffer, which a plain Uint8Array is not guaranteed to be.
    await stream.write(new Blob([bytes]))
    await stream.close()
  }

  async deleteFile (filename: string): Promise<void> {
    await this.dir.removeEntry(filename)
  }

  /**
   * Renames the `from` file to the `to`.
   *
   * Copies bytes rather than text: OPFS has no rename, so this is a copy and a
   * delete, and routing it through `loadFile` would have destroyed any file
   * that is not UTF-8 (#385).
   */
  async renameFile (from: string, to: string): Promise<void> {
    const bytes = await this.loadBytes(from)
    if (await this.fileExists(to)) {
      await this.deleteFile(to)
    }
    await this.saveBytes(to, bytes)
    await this.deleteFile(from)
  }
}

export default OPFSFileSystem