// Patch notes shown to a user the first time they open a new version of Scamper
// (issue #306). The IDE records the last version a user has seen in its config
// file; on load it shows notes for every release between that version and the
// current one, then records the current version so they are not shown again.
//
// The notes themselves live in `patch-notes.md` at the root of the repository
// (#565). This module only parses that file: the format is a heading per
// version and a bullet per note, so adding one is appending a line, with no
// syntax to get wrong under the union merge that lets concurrent pull requests
// both append. See docs/releasing.md.

import source from '../../../patch-notes.md?raw'

export interface PatchNote {
  /** The release these notes describe, e.g. '3.5.0', or `NEXT_RELEASE`. */
  version: string
  /** An optional one-line headline for the release. */
  title?: string
  /** User-facing highlights, one bullet each. */
  notes: string[]
}

/**
 * The version an entry carries before its release has been named. It is not a
 * version, so `compareVersions` reads it as NaN and `patchNotesSince` never
 * returns it -- an accumulating entry is invisible to students until the
 * release pull request renames it.
 */
export const NEXT_RELEASE = 'next'

/**
 * The notes in `patch-notes.md`, in the order they are written there.
 *
 * A `# <version>` line starts an entry, a `> <text>` line under one is its
 * headline, and a `- <text>` line is a note. Everything else is ignored,
 * including the HTML comment the file opens with -- which is skipped
 * explicitly rather than by accident, since it contains lines that would
 * otherwise read as notes.
 */
export function parsePatchNotes(source: string): PatchNote[] {
  const entries: PatchNote[] = []
  let inComment = false
  for (const raw of source.split('\n')) {
    const line = raw.trim()
    if (inComment) {
      inComment = !line.endsWith('-->')
      continue
    }
    if (line.startsWith('<!--')) {
      inComment = !line.endsWith('-->')
      continue
    }
    const heading = /^#\s+(.+?)$/.exec(line)
    if (heading !== null) {
      entries.push({ version: heading[1], notes: [] })
      continue
    }
    // A note before any heading has no release to belong to, so it is dropped
    // rather than silently joining whichever entry comes next.
    const entry = entries.at(-1)
    if (entry === undefined) { continue }
    if (line.startsWith('> ') && entry.notes.length === 0) {
      // Only before the first note, which is what "directly under the heading"
      // means; a stray quote further down is prose, not a second headline.
      entry.title = line.slice(2).trim()
    } else if (line.startsWith('- ')) {
      entry.notes.push(line.slice(2).trim())
    }
  }
  return entries
}

export const patchNotes: PatchNote[] = parsePatchNotes(source)

/**
 * The release a version belongs to: `4.4.0-rc.2` is a preview of `4.4.0`, and
 * for patch-note purposes it *is* `4.4.0` (#565).
 *
 * Notes are filed under the release, never under a candidate, so without this
 * a preview build would show none of the notes it exists to let someone read.
 */
export function releaseVersionOf(version: string): string {
  const suffix = version.indexOf('-')
  return suffix === -1 ? version : version.slice(0, suffix)
}

/**
 * Compares two dotted numeric version strings (e.g. '3.5.0'). A release
 * candidate compares as the release it previews. Only numeric components are
 * supported; a non-numeric component compares as NaN, which makes
 * patchNotesSince fall through to showing nothing (a safe default).
 * @returns a negative number if a < b, 0 if equal, a positive number if a > b.
 */
export function compareVersions(a: string, b: string): number {
  const pa = releaseVersionOf(a).split('.')
  const pb = releaseVersionOf(b).split('.')
  const len = Math.max(pa.length, pb.length)
  for (let i = 0; i < len; i++) {
    const da = Number(pa[i] ?? 0)
    const db = Number(pb[i] ?? 0)
    if (da !== db) return da - db
  }
  return 0
}

/**
 * The patch notes a user should see, given the last version they saw and the
 * current app version: every release newer than `lastSeen` and no newer than
 * `current`, sorted newest-first. The `next` entry is never among them, since
 * it compares as NaN against both bounds.
 */
export function patchNotesSince(
  lastSeen: string,
  current: string,
): PatchNote[] {
  return patchNotes
    .filter(
      (n) =>
        compareVersions(n.version, lastSeen) > 0 &&
        compareVersions(n.version, current) <= 0,
    )
    .sort((x, y) => compareVersions(y.version, x.version))
}

/**
 * The notes filed under `version`, if any. Matching lives here rather than at
 * the call site so that a release candidate finds its release's notes; see
 * `releaseVersionOf`.
 */
export function patchNotesFor(version: string): PatchNote[] {
  const release = releaseVersionOf(version)
  return patchNotes.filter((n) => n.version === release)
}
