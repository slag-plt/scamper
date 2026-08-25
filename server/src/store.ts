import type { FileEntry } from '../../src/fs/fs'
import { previewOf, type FileStore } from './stores'

/**
 * Files in memory, per user.
 *
 * Used by the tests, which drive the real route layer against it, and by a
 * checkout with no database configured. `MariaDbFileStore` is the durable one.
 *
 * Async like its counterpart even though nothing here waits: the interface is
 * shaped by the database, and a synchronous variant would only let a caller
 * forget an `await` that matters in production.
 */
export class MemoryFileStore implements FileStore {
  private readonly users = new Map<string, Map<string, Uint8Array>>()

  list(userId: string): Promise<FileEntry[]> {
    const files = this.users.get(userId) ?? new Map<string, Uint8Array>()
    const entries = [...files.entries()].map(([name, contents]) => ({
      name,
      preview: previewOf(name, contents),
      isDirectory: false,
    }))

    return Promise.resolve(entries.sort((a, b) => a.name.localeCompare(b.name)))
  }

  read(userId: string, name: string): Promise<Uint8Array | undefined> {
    return Promise.resolve(this.users.get(userId)?.get(name))
  }

  write(userId: string, name: string, bytes: Uint8Array): Promise<void> {
    this.filesOf(userId).set(name, bytes)
    return Promise.resolve()
  }

  remove(userId: string, name: string): Promise<boolean> {
    return Promise.resolve(this.users.get(userId)?.delete(name) ?? false)
  }

  rename(userId: string, from: string, to: string): Promise<boolean> {
    const files = this.filesOf(userId)
    const contents = files.get(from)
    if (contents === undefined) return Promise.resolve(false)

    files.delete(from)
    files.set(to, contents)
    return Promise.resolve(true)
  }

  /** @returns `userId`'s files, creating the namespace on first write. */
  private filesOf(userId: string): Map<string, Uint8Array> {
    let files = this.users.get(userId)
    if (files === undefined) {
      files = new Map<string, Uint8Array>()
      this.users.set(userId, files)
    }
    return files
  }
}
