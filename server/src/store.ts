// src/fs/fs.ts is the contract both halves implement, and the one module in
// src/ the server may import outright: sharing `isHiddenName` is what keeps
// this backend and OPFS agreeing on what counts as a user's own file.
import { isHiddenName, type FileEntry } from '../../src/fs/fs'

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
      //
      // Dotted names carry no preview, matching src/fs/opfs.ts and
      // src/fs/node.ts. This is not cosmetic: a file's saved history lives
      // beside it as `.{filename}.history` and holds up to fifty whole
      // snapshots, so previewing one would put every past version of every
      // file into a listing nothing displays them in.
      preview: isHiddenName(name)
        ? null
        : contents.split('\n').slice(0, PREVIEW_LINES).join('\n'),
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
