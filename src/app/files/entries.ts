import { extensionOf, isBinaryName, isImageName } from '../../fs/fs'
import { fileOfHistory } from '../../history/flat-file'
import { LEGACY_CONFIG_FILENAME } from '../web/ide-config'
import { SWAP_SUFFIX, type StorageEntry } from './opfs-direct'

/*
 * Turning a raw storage listing into something a student can read.
 *
 * Split out from the component and kept free of the DOM so it unit-tests with
 * no fakes at all: every function here is a value in, a string or an array
 * out.
 */

const SIZE_UNITS = ['B', 'KB', 'MB', 'GB', 'TB']

/**
 * @returns `bytes` in the largest unit that leaves a number worth reading,
 *          e.g. `0 B`, `1.5 KB`, `212 MB`
 */
export function formatSize(bytes: number): string {
  if (!Number.isFinite(bytes) || bytes < 0) return '—'
  let size = bytes
  let unit = 0
  while (size >= 1024 && unit < SIZE_UNITS.length - 1) {
    size /= 1024
    unit++
  }
  // Whole bytes are whole; above that one decimal is enough, and past 10 of a
  // unit even that is noise.
  const shown =
    unit === 0
      ? Math.round(size).toString()
      : size < 10
        ? size.toFixed(1)
        : Math.round(size).toString()
  return `${shown} ${SIZE_UNITS[unit]}`
}

/**
 * @returns when the file last changed, in the reader's own locale, or '' if
 *          the browser did not say
 */
export function formatWhen(lastModified: number): string {
  if (!Number.isFinite(lastModified) || lastModified <= 0) return ''
  return new Date(lastModified).toLocaleString()
}

/**
 * @returns a plain-language line saying what `entry` is.
 *
 * The internal names matter most here: this is the only page that shows them,
 * and a student staring at `.hello.scm.history` or `hello.scm.crswap` has no
 * way to know which of them is safe to delete.
 */
export function describe(entry: StorageEntry): string {
  if (entry.isDirectory) return 'Folder'
  const name = entry.name
  const historyOf = fileOfHistory(name)
  if (historyOf !== null) return `Save history for ${historyOf}`
  if (name.endsWith(SWAP_SUFFIX)) {
    const of = name.slice(0, -SWAP_SUFFIX.length)
    return `Left over from an interrupted save of ${of}`
  }
  if (name === LEGACY_CONFIG_FILENAME) {
    return 'Settings file left by an older version of Scamper'
  }
  if (extensionOf(name) === 'scm') return 'Scamper program'
  if (isImageName(name)) return 'Image'
  if (isBinaryName(name)) return 'Data file'
  return 'Text file'
}

/**
 * @returns `entries` sorted directories first and then by name, matching the
 *          order the IDE's file drawer uses
 */
export function sortEntries(entries: StorageEntry[]): StorageEntry[] {
  return [...entries].sort((a, b) => {
    if (a.isDirectory !== b.isDirectory) return a.isDirectory ? -1 : 1
    return a.name.localeCompare(b.name)
  })
}

/** @returns the total size of `entries`, in bytes. */
export function totalSize(entries: StorageEntry[]): number {
  return entries.reduce((sum, entry) => sum + entry.size, 0)
}
