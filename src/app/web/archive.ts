import type * as FS from '../../fs'
import { isUserFile } from '../../fs/fs'

// Bundles the user's files into a single zip so a student can take their work
// with them (issue #42). Internal dotfiles are left out regardless of whether
// the drawer is currently showing them (#178) -- they are Scamper's state, not
// the student's work. Directories are skipped as well: the FS interface only
// lists the root, so there is no way to read their contents.
//
// The DOM work of handing the zip to the browser lives at the call site
// (IdeApp.vue); this module stays framework-free and unit-testable.

/** The archive's date-stamped name, e.g. `scamper-files-2026-08-07.zip`. */
export function archiveFilename(now: Date = new Date()): string {
  const pad = (n: number) => n.toString().padStart(2, '0')
  // Local time, not UTC: the stamp should match the day the student sees on
  // their own clock.
  const date = `${now.getFullYear().toString()}-${pad(now.getMonth() + 1)}-${pad(now.getDate())}`
  return `scamper-files-${date}.zip`
}

/**
 * Builds a zip archive of every user file in `fs`.
 * @returns a promise that resolves to the archive's contents
 * @throws if a listed file cannot be read
 */
export async function buildArchive(fs: FS.t): Promise<Blob> {
  // JSZip is 96KB (28KB gzipped) and exporting is a rare, deliberate action, so
  // it is fetched on first use rather than bundled into the IDE's initial load.
  const { default: JSZip } = await import('jszip')
  const zip = new JSZip()
  const entries = (await fs.getFileList()).filter(isUserFile)
  for (const entry of entries) {
    try {
      zip.file(entry.name, await fs.loadFile(entry.name))
    } catch (e) {
      // A file can vanish between the listing and the read. Name it, since the
      // underlying error usually doesn't.
      const reason = e instanceof Error ? e.message : String(e)
      throw new Error(`Could not read "${entry.name}": ${reason}`, { cause: e })
    }
  }
  return zip.generateAsync({ type: 'blob', compression: 'DEFLATE' })
}
