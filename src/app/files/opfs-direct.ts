import type { Bytes } from '../../fs/fs'
import { opfsWriter } from '../../fs/opfs-writer'
import type JSZip from 'jszip'

/*
 * A direct, deliberately minimal view of the browser's private file storage
 * (OPFS), for the rescue page at `files.html` (issue #130).
 *
 * It does **not** reuse `src/fs/opfs.ts`, the IDE's file system, and the four
 * reasons all bite precisely when storage is what has gone wrong:
 *
 *  + `getFileList` (opfs.ts:52) builds a preview by calling `file.text()`
 *    (opfs.ts:99-107) -- a whole read of every user file, to keep five lines
 *    of it. On the storage this page exists to rescue, listing the directory
 *    is itself the hazard. `list` below asks the handle for `.size` and
 *    `.lastModified` and never touches contents.
 *  + It hides `<name>.crswap` (opfs.ts:5, 70). Chromium leaves one behind for
 *    good when a tab dies mid-save, which is exactly the wedged state this
 *    page has to be able to show and delete.
 *  + `FileEntry` (src/fs/fs.ts:13-17) carries no size, the most useful column
 *    here.
 *  + `deleteFile` (opfs.ts:178-180) calls `removeEntry` without
 *    `{ recursive: true }`, so it cannot remove a non-empty directory.
 *
 * Staying off `src/fs/index.ts` also keeps this page's import graph tiny,
 * which is the point: it has to load on a Scamper whose IDE will not.
 */

/** The suffix Chromium gives the swap file backing an open writable. */
export const SWAP_SUFFIX = '.crswap'

/** One entry at the root of storage, as this page needs to show it. */
export interface StorageEntry {
  name: string
  kind: 'file' | 'directory'
  /** Bytes on disk; 0 for a directory, whose size would mean walking it. */
  size: number
  /** Epoch milliseconds, or 0 where the browser will not say. */
  lastModified: number
  /**
   * `kind === 'directory'`, restated because that is how the rest of Scamper
   * spells it (`FileEntry`). `list` is the only producer, so the two cannot
   * disagree.
   */
  isDirectory: boolean
}

/** How much of the origin's storage is in use, as the browser estimates it. */
export interface StorageUsage {
  used: number
  quota: number
}

/**
 * @returns the storage manager, or null on a host without one -- an old
 *          browser, a sandboxed frame, jsdom. The DOM lib calls it
 *          always-present; this page has to render a sentence rather than a
 *          stack trace where it is not.
 */
function storageManager(): StorageManager | null {
  return (navigator as { storage?: StorageManager }).storage ?? null
}

/** @returns the OPFS root directory. */
export function openRoot(): Promise<FileSystemDirectoryHandle> {
  const storage = storageManager()
  if (storage === null || typeof storage.getDirectory !== 'function') {
    return Promise.reject(
      new Error('this browser gives Scamper no private file storage'),
    )
  }
  return storage.getDirectory()
}

/**
 * @returns `dir`'s entries, name and handle apiece.
 *
 * N.B., `entries()` is missing from the DOM lib's `FileSystemDirectoryHandle`
 * but present in every browser with OPFS, so its shape is declared here once
 * rather than spread as `any` through the callers.
 * https://developer.mozilla.org/en-US/docs/Web/API/FileSystemDirectoryHandle/entries
 */
function iterate(
  dir: FileSystemDirectoryHandle,
): AsyncIterable<[string, FileSystemHandle]> {
  return (
    dir as unknown as {
      entries: () => AsyncIterable<[string, FileSystemHandle]>
    }
  ).entries()
}

/**
 * Lists `dir`, metadata only.
 *
 * Nothing is hidden: dotfiles and leftover `.crswap` files are what a stuck
 * Scamper is made of, so this is the one listing that must show them. Nothing
 * is read either -- `getFile()` hands back a lazy view of the bytes on disk,
 * so `.size` and `.lastModified` cost no memory however large the file is.
 *
 * @returns the entries in the order storage gave them; see `sortEntries`.
 */
export async function list(
  dir: FileSystemDirectoryHandle,
): Promise<StorageEntry[]> {
  const entries: StorageEntry[] = []
  for await (const [name, handle] of iterate(dir)) {
    if (handle.kind === 'directory') {
      entries.push({
        name,
        kind: 'directory',
        size: 0,
        lastModified: 0,
        isDirectory: true,
      })
      continue
    }
    let size = 0
    let lastModified = 0
    try {
      const file = await (handle as FileSystemFileHandle).getFile()
      size = file.size
      lastModified = file.lastModified
    } catch {
      // An entry can vanish between the listing and the stat -- a swap file
      // for a write that has since closed. List it anyway: a name the student
      // can try to delete beats a listing that failed outright.
    }
    entries.push({ name, kind: 'file', size, lastModified, isDirectory: false })
  }
  return entries
}

/**
 * @returns the file's contents as a blob, without reading them into memory.
 *          A `File` is a lazy view of the bytes on disk, which is what makes
 *          this safe to hand straight to a download or to the zip.
 */
export async function readBlob(
  dir: FileSystemDirectoryHandle,
  name: string,
): Promise<Blob> {
  const handle = await dir.getFileHandle(name)
  return handle.getFile()
}

/**
 * @returns the file's bytes, in memory.
 *
 * Only the rename fallback below needs these; everything else takes the blob
 * above and never holds a copy of the file.
 */
export async function readBytes(
  dir: FileSystemDirectoryHandle,
  name: string,
): Promise<Bytes> {
  const handle = await dir.getFileHandle(name)
  const file = await handle.getFile()
  return new Uint8Array(await file.arrayBuffer())
}

/**
 * Deletes `name` from `dir`, contents and all.
 *
 * `{ recursive: true }` is what the IDE's `deleteFile` leaves off, so a
 * non-empty directory could not be removed there. Here a directory is one of
 * the things a student may need to get rid of.
 */
export async function remove(
  dir: FileSystemDirectoryHandle,
  name: string,
): Promise<void> {
  await dir.removeEntry(name, { recursive: true })
}

/** A file handle in a browser that can rename in place. */
interface MovableFileHandle {
  move: (name: string) => Promise<void>
}

/**
 * Renames `from` to `to`, replacing `to` if it exists.
 *
 * `move()` where the browser has it, because it renames in place: the file
 * this page exists to rescue may be far too big to pull through memory. The
 * fallback is a copy and a delete, which is all OPFS otherwise offers -- done
 * through the worker in `opfs-writer.ts`, the one write that works everywhere
 * (Safari before 26 has no `createWritable` either, #429).
 */
export async function rename(
  dir: FileSystemDirectoryHandle,
  from: string,
  to: string,
): Promise<void> {
  const handle = await dir.getFileHandle(from)
  const movable = handle as Partial<MovableFileHandle>
  if (typeof movable.move === 'function') {
    await movable.move(to)
    return
  }
  await opfsWriter.write(to, await readBytes(dir, from))
  await remove(dir, from)
}

/** Adds everything under `dir` to `zip`, at `prefix`. */
async function addAll(
  zip: JSZip,
  dir: FileSystemDirectoryHandle,
  prefix: string,
): Promise<void> {
  for await (const [name, handle] of iterate(dir)) {
    const path = prefix + name
    if (handle.kind === 'directory') {
      await addAll(zip, handle as FileSystemDirectoryHandle, `${path}/`)
    } else {
      try {
        zip.file(path, await (handle as FileSystemFileHandle).getFile())
      } catch {
        // As in `list`: an entry can go away underneath us. One unreadable
        // file must not cost the student the rest of the archive.
      }
    }
  }
}

/**
 * Builds a zip of *everything* in storage.
 *
 * Everything, deliberately: the IDE's exporter filters with `isUserFile`
 * (archive.ts:32), which drops dotfiles and directories. This is the copy a
 * student takes before deleting, so it leaves nothing behind.
 *
 * @returns a promise that resolves to the archive's contents
 */
export async function zipAll(dir: FileSystemDirectoryHandle): Promise<Blob> {
  // JSZip is 96KB (28KB gzipped) and exporting is a rare, deliberate action,
  // so it is fetched on first use rather than bundled into this page's load.
  const { default: Zip } = await import('jszip')
  const zip = new Zip()
  await addAll(zip, dir, '')
  return zip.generateAsync({ type: 'blob', compression: 'DEFLATE' })
}

/**
 * @returns what the browser says this origin is using and is allowed, or null
 *          where it will not estimate
 */
export async function usage(): Promise<StorageUsage | null> {
  const storage = storageManager()
  if (storage === null || typeof storage.estimate !== 'function') {
    return null
  }
  const estimate = await storage.estimate()
  return { used: estimate.usage ?? 0, quota: estimate.quota ?? 0 }
}
