// Patch notes shown to a user the first time they open a new version of Scamper
// (issue #306). The IDE records the last version a user has seen in its config
// file; on load it shows notes for every release between that version and the
// current one, then records the current version so they are not shown again.
//
// To announce a release: add an entry here whose `version` matches the app
// version (package.json). Keep `notes` short and user-facing -- these are what
// students read, not a full changelog. Order does not matter; entries are
// sorted newest-first when displayed.

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
    version: '3.6.0',
    // N.B., one entry per *release*, not per change: the colors work (#344) and
    // the argument-order work (#345) both ship in 3.6.0, so their notes belong
    // in the same block.
    title: 'Library cleanup',
    notes: [
      'Every function that takes a color now accepts a color name, an rgb value, or an hsv value — so (solid-square 100 (rgb 255 0 0)) works, where before only "red" did.',
      'The old `color` function has been removed. Use `rgb` instead: (rgb 255 0 0) in place of (color 255 0 0 255).',
      'index-of now takes the value first: (index-of "b" lst), matching how member and assoc read.',
      'for-range now takes its function first: (for-range f 0 10), like map, filter, and for-each.',
      'Maps built with { ... } now have functions to work with them: hash-ref, hash-set, hash-set!, hash-keys, and more.',
    ],
  },
  {
    version: '3.5.0',
    title: 'Editor upgrades',
    notes: [
      'Hover a function name in the editor to read its documentation, jump to its definition, or find where it is used.',
      'A status bar now shows the syntactic form enclosing your cursor.',
      "In-app dialogs replace the browser's built-in pop-ups and follow the light/dark theme.",
      'Inline comments inside an expression no longer cause spurious parse errors.',
    ],
  },
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
