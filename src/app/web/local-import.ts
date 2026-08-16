// Carrying browser-stored files into an account, the first time someone signs
// in on a machine.
//
// Signing in swaps the file system out (see server-session.ts), so work saved
// in this browser before there was an account stops being listed. Nothing is
// destroyed -- it is still in the browser's own storage -- but "my files are
// gone" is what it looks like, and a student has no way to reach them again.
// So the first sign-in on a machine offers to copy them across.
//
// Copying rather than moving: the local copies stay where they are. If the
// import goes wrong halfway, or the account turns out to be the wrong one,
// nothing has been lost.

import type { FS } from '../../fs/fs'
import { isUserFile } from '../../fs/fs'
import OPFSFileSystem from '../../fs/opfs'

/** What an import did, for the message afterwards. */
export interface ImportResult {
  copied: string[]
  /** Files that arrived under a new name because the account had that one. */
  renamed: { from: string; to: string }[]
}

/** The marker added to a name the account was already using. */
const SUFFIX = 'from this browser'

/**
 * @returns a name not in `taken`, derived from `name`
 *
 * A student who had `homework.scm` in the browser *and* in their account has
 * two different files with one name, and neither may be thrown away. The copy
 * says where it came from, which is the thing they need to know to tell them
 * apart.
 */
export function uniqueName(name: string, taken: ReadonlySet<string>): string {
  if (!taken.has(name)) return name

  const dot = name.lastIndexOf('.')
  const stem = dot <= 0 ? name : name.slice(0, dot)
  const extension = dot <= 0 ? '' : name.slice(dot)

  const first = `${stem} (${SUFFIX})${extension}`
  if (!taken.has(first)) return first

  for (let n = 2; ; n++) {
    const candidate = `${stem} (${SUFFIX} ${n.toString()})${extension}`
    if (!taken.has(candidate)) return candidate
  }
}

/**
 * @returns the names of the user's files in this browser's own storage
 *
 * Opens local storage directly rather than through the global backend, which
 * by this point is the server's.
 */
export async function localFileNames(): Promise<string[]> {
  try {
    const local = await OPFSFileSystem.create()
    const files = await local.getFileList()
    return files.filter(isUserFile).map((file) => file.name)
  } catch {
    // No local storage, or nothing in it. Either way there is nothing to offer.
    return []
  }
}

/**
 * Copies this browser's files into `target`, renaming any whose name is taken.
 *
 * @returns what was copied, and under what names
 */
export async function importLocalFiles(target: FS): Promise<ImportResult> {
  const local = await OPFSFileSystem.create()
  const names = await localFileNames()

  const taken = new Set(
    (await target.getFileList()).filter(isUserFile).map((file) => file.name),
  )
  const result: ImportResult = { copied: [], renamed: [] }

  for (const name of names) {
    const destination = uniqueName(name, taken)
    await target.saveFile(destination, await local.loadFile(name))
    // Added as we go, so two local files that would collide with each other
    // after renaming do not land on the same name.
    taken.add(destination)

    result.copied.push(destination)
    if (destination !== name) result.renamed.push({ from: name, to: destination })
  }

  return result
}
