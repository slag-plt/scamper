// Patch notes shown to a user the first time they open a new version of Scamper
// (issue #306). The IDE records the last version a user has seen in its config
// file; on load it shows notes for every release between that version and the
// current one, then records the current version so they are not shown again.
//
// To announce a release: add an entry here whose `version` matches the app
// version (package.json). Keep `notes` short and user-facing -- these are what
// students read, not a full changelog. Order does not matter; entries are
// sorted newest-first when displayed.
//
// Write the entry as the work lands, not at release time: the version here may
// name a release that has not happened yet, which is how notes accumulate for
// the next one. CI requires an entry for a minor or major release and lets a
// patch release go without one, since a bug fix does not deserve a modal in
// front of every student. See RELEASING.md.

export interface PatchNote {
  /** The release version these notes describe, e.g. '3.5.0'. */
  version: string
  /** An optional one-line headline for the release. */
  title?: string
  /** User-facing highlights, one bullet each. */
  notes: string[]
}

export const patchNotes: PatchNote[] = [
  {
    version: '4.1.0',
    notes: [
      'The output now opens in a pane beside your code rather than in a window floating over it.',
      'The separate run window has been removed.',
      'The View menu can now show the internal files Scamper keeps, such as saved file histories, which open read-only.',
      'Pressing Run on a file with no code in it no longer fails silently.',
      'Audio pipelines now accept a sample, so a sample-node can drive one.',
      'Your file now runs by itself a moment after you stop typing; turn that off under Run > Live Evaluation.'
    ]
  },
  {
    title: 'AY 2026–2027 release',
    version: '4.0.0',
    notes: [
      'Major updates to Scamper for the 26–27 academic year!',
      'Scamper is now backed by a server for cloud-based file sharing. See the current CSC 151 instructor for an account.',
      'The UI has been overhauled to better reflect the feature set of a modern IDE.',
      'Both the language and libraries have been revised heavily. See the documentation for relevant updates.'
    ]
  }
]

/**
 * Compares two dotted numeric version strings (e.g. '3.5.0'). Only numeric
 * components are supported; a non-numeric component compares as NaN, which
 * makes patchNotesSince fall through to showing nothing (a safe default).
 * @returns a negative number if a < b, 0 if equal, a positive number if a > b.
 */
export function compareVersions(a: string, b: string): number {
  const pa = a.split('.')
  const pb = b.split('.')
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
 * `current`, sorted newest-first.
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
