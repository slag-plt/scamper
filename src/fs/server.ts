import type { FS, FileEntry } from './fs'

/**
 * A file system backed by the Scamper file server, for a user who is logged
 * in. The logged-out default stays OPFS -- see `src/fs/index.ts`.
 *
 * The one thing to understand before changing this class is that `fileExists`
 * cannot make a request. `src/fs/opfs.ts` documents it as a hot path: module
 * resolution, import steps, and the `file-exists?` primitive a student can call
 * in a loop all run through it. A round trip per call would turn a loop over
 * a handful of files into a visibly slow program. So the file *names* are
 * cached, and only the operations a person triggers -- opening the file drawer,
 * saving, deleting -- go to the network.
 *
 * The cache tracks this tab's own writes. A change made on another device shows
 * up at the next `getFileList()`, which the file drawer performs whenever it is
 * opened.
 */
export class ServerFileSystem implements FS {
  /** API root, e.g. `https://host/api/v1`, without a trailing slash. */
  private readonly baseUrl: string

  /** Names of every file on the server, or null while the cache is cold. */
  private names: Set<string> | null = null

  /** The in-flight listing request, so a cold cache is only ever filled once. */
  private pending: Promise<FileEntry[]> | null = null

  private constructor(baseUrl: string) {
    this.baseUrl = baseUrl.replace(/\/+$/, '')
  }

  /**
   * @param baseUrl the API root the deployment advertises, as read from
   *                `src/fs/config.ts`
   * @returns a file system that reads and writes the given server
   */
  static create(baseUrl: string): ServerFileSystem {
    return new ServerFileSystem(baseUrl)
  }

  /**
   * Performs a request against the file server, failing loudly on any non-2xx
   * reply so a caller never mistakes an error page for file contents.
   * @param path the path below the API root, e.g. `fs/files`
   * @param body a value to send as a JSON request body, if any
   */
  private async request(
    method: string,
    path: string,
    body?: unknown,
  ): Promise<Response> {
    const response = await fetch(`${this.baseUrl}/${path}`, {
      method,
      // The session cookie has to ride along, and it is a cross-origin cookie
      // whenever the server is not also the static host.
      credentials: 'include',
      headers:
        body === undefined ? undefined : { 'Content-Type': 'application/json' },
      body: body === undefined ? undefined : JSON.stringify(body),
    })

    if (!response.ok) {
      throw new Error(
        `File server ${method} ${path} failed: ${response.status.toString()} ${response.statusText}`,
      )
    }

    return response
  }

  /**
   * Fetches the listing from the server and refreshes the name cache from it.
   * @returns the entries the server reports
   */
  private async fetchFileList(): Promise<FileEntry[]> {
    const response = await this.request('GET', 'fs/files')
    const body: unknown = await response.json()
    const files = readFileEntries(body)
    this.names = new Set(files.map((file) => file.name))
    return files
  }

  /** @returns the list of files held for this user on the server */
  async getFileList(): Promise<FileEntry[]> {
    // Always fresh: a person opening the file drawer is asking to see the
    // current state, and it is not a hot path.
    return await this.fetchFileList()
  }

  /** @returns true iff the given file exists, answered from cache when warm */
  async fileExists(filename: string): Promise<boolean> {
    if (this.names === null) {
      // Several cold lookups can be outstanding at once -- a program importing
      // three modules at once, say -- and they should cost one request, not
      // three. Whoever arrives first starts it; the rest wait on the same one.
      this.pending ??= this.fetchFileList().finally(() => {
        this.pending = null
      })
      await this.pending
    }

    return this.names?.has(filename) ?? false
  }

  /** @returns the contents of the given file, assumed to exist */
  async loadFile(filename: string): Promise<string> {
    const response = await this.request('GET', `fs/files/${encode(filename)}`)
    const body: unknown = await response.json()

    if (
      typeof body !== 'object' ||
      body === null ||
      typeof (body as { contents: unknown }).contents !== 'string'
    ) {
      throw new Error(`File server returned no contents for ${filename}`)
    }

    return (body as { contents: string }).contents
  }

  /** Saves `contents` to the given file, creating it if it does not exist */
  async saveFile(filename: string, contents: string): Promise<void> {
    await this.request('PUT', `fs/files/${encode(filename)}`, { contents })
    // Keep the cache consistent with our own writes, so a program that creates
    // a file and immediately asks whether it exists gets the right answer.
    this.names?.add(filename)
  }

  /** Deletes the given file */
  async deleteFile(filename: string): Promise<void> {
    await this.request('DELETE', `fs/files/${encode(filename)}`)
    this.names?.delete(filename)
  }

  /** Renames `from` to `to`, overwriting `to` if it already exists */
  async renameFile(from: string, to: string): Promise<void> {
    // One request rather than a copy-then-delete pair, so an interruption
    // cannot leave the user with two copies or none.
    await this.request('POST', 'fs/rename', { from, to })
    this.names?.delete(from)
    this.names?.add(to)
  }
}

/** Escapes a filename for use as a single path segment. */
function encode(filename: string): string {
  return encodeURIComponent(filename)
}

/**
 * Validates a listing reply rather than trusting its shape, so a proxy's error
 * page or an older server surfaces as a clear failure here instead of as
 * undefined names somewhere downstream.
 * @param body the parsed reply from the listing endpoint
 * @returns the entries it holds
 */
function readFileEntries(body: unknown): FileEntry[] {
  const files =
    typeof body === 'object' && body !== null
      ? (body as { files: unknown }).files
      : undefined

  if (!Array.isArray(files)) {
    throw new Error('File server returned a malformed file listing')
  }

  return files.map((entry: unknown) => {
    if (
      typeof entry !== 'object' ||
      entry === null ||
      typeof (entry as { name: unknown }).name !== 'string'
    ) {
      throw new Error('File server returned a malformed file entry')
    }

    const { name, preview, isDirectory } = entry as {
      name: string
      preview: unknown
      isDirectory: unknown
    }

    return {
      name,
      preview: typeof preview === 'string' ? preview : null,
      isDirectory: isDirectory === true,
    }
  })
}

export default ServerFileSystem
