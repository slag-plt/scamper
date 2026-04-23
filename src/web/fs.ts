/** An entry from the file system */
export interface FileEntry {
  name: string
  preview: string | null
  isDirectory: boolean
}

/**
 * A wrapper around the OPFS API that simplifies access to files with a
 * higher-level API.
 */
export class OPFSFileSystem {
  private root?: FileSystemDirectoryHandle

  /** @returns a new file system instance for accessing the OPFS */
  static async create(): Promise<OPFSFileSystem> {
    const ret = new OPFSFileSystem()
    ret.root = await navigator.storage.getDirectory()
    return ret
  }

  /** @return the list of files found at the root of the file system */
  async getFileList(): Promise<FileEntry[]> {
    const root = this.root
    if (root === undefined) {
      throw new Error('File system root is not initialized')
    }
  
    const fileEntries: FileEntry[] = []

    // N.B., this.root doesn't have an entries field according to the type
    // checker... but it does!
    // https://developer.mozilla.org/en-US/docs/Web/API/FileSystemDirectoryHandle/entries
    const rootWithEntries = root as FileSystemDirectoryHandle & {
      entries(): AsyncIterable<[string, FileSystemHandle]>
    }
    for await (const [, handle] of rootWithEntries.entries()) {
      const isDirectory = handle.kind === 'directory'
      let preview: string | null = null

      if (!isDirectory) {
        try {
          preview = await this.getFilePreview(handle as FileSystemFileHandle)
        } catch {
          preview = null
        }
      }

      fileEntries.push({
        name: handle.name,
        preview,
        isDirectory,
      })
    }

    return fileEntries.sort((a, b) => {
      if (a.isDirectory && !b.isDirectory) return -1
      if (!a.isDirectory && b.isDirectory) return 1
      return a.name.localeCompare(b.name)
    })
  }

  /** @return a preview (prefix) of the file denoted by the given handle */
  async getFilePreview(fileHandle: FileSystemFileHandle): Promise<string> {
    try {
      const file = await fileHandle.getFile()
      const text = await file.text()
      return text.split('\n').slice(0, 5).join('\n')
    } catch (e) {
      throw new Error(`Failed to get file preview: ${String(e)}`, { cause: e })
    }
  }

  /** @return true iff the given file exists */
  async fileExists(filename: string): Promise<boolean> {
    const list = await this.getFileList()
    return list.some((file) => file.name === filename)
  }


  /** @return the contents of the given file, assumed to exist */
  async loadFile (filename: string): Promise<string> {
  const root = this.root
  if (root === undefined) {
  throw new Error('File system root is not initialized')
  }
    const handle = await root.getFileHandle(filename)
    const file = await handle.getFile()
    return await file.text()
  }

  /** Saves `contents` to the given file, creating it if it doesn't already exist */
  async saveFile (filename: string, contents: string): Promise<void> {
  const root = this.root
  if (root === undefined) {
  throw new Error('File system root is not initialized')
  }
    const handle = await root.getFileHandle(filename, { create: true })
    const stream = await handle.createWritable()
    await stream.write(contents)
    await stream.close()
  }

  async deleteFile (filename: string): Promise<void> {
    const root = this.root
    if (root === undefined) {
    throw new Error('File system root is not initialized')
    }
    await root.removeEntry(filename)
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