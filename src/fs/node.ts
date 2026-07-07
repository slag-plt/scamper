import fs from 'node:fs/promises'
import path from 'node:path'
import { ScamperError } from '../lpm/error'
import type { FS, FileEntry } from './fs'

/**
 * An implementation of FS backed by Node's `fs` module, rooted at a
 * particular directory on the local file system.
 */
export class NodeFileSystem implements FS {
  private root: string

  private constructor(root: string) {
    this.root = root
  }

  /**
   * @param root the directory to root this file system at, created if it
   *             doesn't already exist
   * @returns a new file system instance for accessing files under `root`
   */
  static async create(root: string): Promise<NodeFileSystem> {
    await fs.mkdir(root, { recursive: true })
    return new NodeFileSystem(path.resolve(root))
  }

  /** @return the path to `filename` within this file system's root */
  private resolve(filename: string): string {
    return path.join(this.root, filename)
  }

  /** @return the list of files found at the root of the file system */
  async getFileList(): Promise<FileEntry[]> {
    const entries = await fs.readdir(this.root, { withFileTypes: true })
    const fileEntries: FileEntry[] = []

    for (const entry of entries) {
      const isDirectory = entry.isDirectory()
      let preview: string | null = null

      if (!isDirectory) {
        try {
          preview = await this.getFilePreview(entry.name)
        } catch {
          preview = null
        }
      }

      fileEntries.push({
        name: entry.name,
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

  /** @return a preview (prefix) of the given file */
  private async getFilePreview(filename: string): Promise<string> {
    try {
      const text = await fs.readFile(this.resolve(filename), 'utf-8')
      return text.split('\n').slice(0, 5).join('\n')
    } catch (e) {
      throw new ScamperError('Runtime', `Failed to get file preview: ${String(e)}`)
    }
  }

  /** @return true iff the given file exists */
  async fileExists(filename: string): Promise<boolean> {
    try {
      await fs.access(this.resolve(filename))
      return true
    } catch {
      return false
    }
  }

  /** @return the contents of the given file, assumed to exist */
  async loadFile(filename: string): Promise<string> {
    return await fs.readFile(this.resolve(filename), 'utf-8')
  }

  /** Saves `contents` to the given file, creating it if it doesn't already exist */
  async saveFile(filename: string, contents: string): Promise<void> {
    await fs.writeFile(this.resolve(filename), contents, 'utf-8')
  }

  async deleteFile(filename: string): Promise<void> {
    await fs.unlink(this.resolve(filename))
  }

  /** Renames the `from` file to the `to`, overwriting `to` if it exists. */
  async renameFile(from: string, to: string): Promise<void> {
    await fs.rename(this.resolve(from), this.resolve(to))
  }
}

export default NodeFileSystem
