// Types only -- src/ is browser code, and the ESLint boundary allows exactly
// this: the FS contract is the shared vocabulary between the two halves.
import type { FileEntry } from '../../src/fs/fs'

/** How many leading lines of a file the listing carries as its preview. */
const PREVIEW_LINES = 5

/**
 * A stub file store: in memory, no authentication, one shared namespace.
 *
 * This exists so the client seam in `src/fs/server.ts` has something real to
 * talk to while the durable half is built. MariaDB-backed, per-user storage
 * behind BetterAuth replaces it (issue #357); the routes in `api.ts` are the
 * part meant to survive that swap.
 */
export class FileStore {
  private readonly files = new Map<string, string>()

  /**
   * @returns every file, ordered the way the file drawer expects: directories
   *          first, then by name. Nothing here creates directories, but the
   *          ordering matches `src/fs/opfs.ts` so the two backends agree.
   */
  list(): FileEntry[] {
    const entries = [...this.files.entries()].map(([name, contents]) => ({
      name,
      // Computed here rather than by the client, which is the point of having
      // a server: a listing costs one request instead of one read per file.
      preview: contents.split('\n').slice(0, PREVIEW_LINES).join('\n'),
      isDirectory: false,
    }))

    return entries.sort((a, b) => a.name.localeCompare(b.name))
  }

  /** @returns the contents of `name`, or undefined if it does not exist */
  read(name: string): string | undefined {
    return this.files.get(name)
  }

  /** Saves `contents` to `name`, creating it if it does not already exist. */
  write(name: string, contents: string): void {
    this.files.set(name, contents)
  }

  /** @returns true iff `name` existed and was removed */
  remove(name: string): boolean {
    return this.files.delete(name)
  }

  /**
   * Renames `from` to `to`, overwriting `to` if it exists.
   * @returns true iff `from` existed
   */
  rename(from: string, to: string): boolean {
    const contents = this.files.get(from)
    if (contents === undefined) return false

    this.files.delete(from)
    this.files.set(to, contents)
    return true
  }
}
