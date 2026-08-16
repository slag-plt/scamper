// The IDE's own settings -- which file was open, which version's patch notes
// have been seen. This is per-machine state about a browsing session, not the
// user's work, so it is deliberately kept out of the file system: on a
// server-backed file system (#357) a config stored as a file would follow the
// user between machines, and opening a laptop would inherit whatever the lab
// computer had open. It would also mean a write to the server on every tab
// hide.
//
// localStorage suits it better than a file for a second reason: saving is
// synchronous, and the handlers that save (pagehide, beforeunload) cannot rely
// on an await completing before the page goes away.
//
// Reads and writes are best-effort. Losing which file was open is not worth
// failing over, so every path degrades to the defaults.

const STORAGE_KEY = 'scamper.config'

/** The name an older build used when this lived in the file system. */
export const LEGACY_CONFIG_FILENAME = '.scamper.config'

export interface Config {
  lastOpenedFilename: string | null
  lastVersionAccessed: string
  /**
   * Whether this browser's files have already been offered to an account.
   * Per-machine because the files are: another computer has its own.
   */
  localFilesOffered: boolean
}

/**
 * @returns localStorage, or null where it isn't usable -- Safari with cookies
 *          blocked, a sandboxed iframe, a non-browser host. The DOM types call
 *          it always-present, but it can be missing outright *or* throw on
 *          access, so both are handled.
 */
function storage(): Storage | null {
  try {
    return (window as { localStorage?: Storage }).localStorage ?? null
  } catch {
    return null
  }
}

/**
 * @returns the stored config, or null if none has been stored (a first run) or
 *          it could not be read
 */
export function readStoredConfig(): Config | null {
  let stored: string | null
  try {
    stored = storage()?.getItem(STORAGE_KEY) ?? null
  } catch {
    return null
  }
  if (stored === null) {
    return null
  }
  try {
    // Only a shape check: a config written by an older build may be missing a
    // newer field, and the caller merges over its defaults to fill the gaps.
    const parsed: unknown = JSON.parse(stored)
    return typeof parsed === 'object' && parsed !== null
      ? (parsed as Config)
      : null
  } catch {
    return null
  }
}

/** Stores the config, doing nothing if storage is unavailable or full. */
export function writeStoredConfig(config: Config): void {
  try {
    storage()?.setItem(STORAGE_KEY, JSON.stringify(config))
  } catch {
    // Unavailable or full -- the IDE works fine without remembering this.
  }
}
