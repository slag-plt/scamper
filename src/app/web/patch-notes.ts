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
    version: '4.2.0',
    notes: [
      'A test in a reading page now looks green when it passes and red when it fails, instead of looking the same either way, and values it prints are in the same typewriter face as the code above them.',
      'circle, solid-circle, and outlined-circle now take the diameter rather than the radius, so (solid-circle 100 "red") is the same size as (solid-square 100 "red"); double the number in a drawing you already have to keep it the size it was.'
    ]
  },
  {
    version: '4.1.1',
    notes: [
      'The Run button no longer shows square corners poking out of it when you hover over it.',
      'A program that has finished is no longer sometimes treated as still running, which could leave the Run button stuck on Stop or stop the rest of a reading page from running.',
      'The documentation now lists `null`, which was missing from it, and shows a constant such as `pi` as a value rather than as a function you would call.',
      'On the documentation page, setting a filter to "and" without ticking anything in it no longer wipes out your search results.',
      'Choosing a file in a reading page\'s example now runs the code that was waiting for it, instead of doing nothing.',
      'A colour name written with capitals, such as (color-name->rgb "RED"), now gives you that colour instead of quietly giving you nothing.',
      'When assoc-ref cannot find a key, it now shows you the key it looked for rather than a piece of jargon.',
      'Saving a file in Safari now works instead of failing with a message about createWritable, so you can use Scamper there without signing in.'
    ]
  },
  {
    version: '4.1.0',
    notes: [
      'Your file can now be shown as a notebook: each definition or expression in its own box, with what it printed underneath, and the comments between them as formatted text.',
      'The output now opens in a pane beside your code rather than in a window floating over it.',
      'The separate run window has been removed.',
      'The View menu can now show the internal files Scamper keeps, such as saved file histories, which open read-only.',
      'Pressing Run on a file with no code in it no longer fails silently.',
      'Audio pipelines now accept a sample, so a sample-node can drive one.',
      'Your file now runs by itself when you open it and a moment after you stop typing; the Run button says "Autorun" while it does, and Run > Live Evaluation turns it off.',
      'Scamper now indents your code as you type, re-indents or reformats a whole file to the standard style, and wraps long expressions in the output instead of running them off the edge.',
      'An @example line in a docstring now gets a checkmark when your code agrees with it and a cross when it does not; Run > Check Examples turns this off.',
      'Files that are not Scamper programs now open properly: plain text without the Scheme squiggles, Markdown and CSV with their own colouring, images as a picture, and other files with a note instead of gibberish.',
      'A string containing a newline or a tab now prints as "a\\nb" on one line instead of breaking across two.',
      'Reformatting your file now keeps the blank lines you left between paragraphs of comments.',
      'Scamper examples can now be embedded in a web page as a live transcript, so a reading can show code beside what it produces.',
      'A struct or define-export now appears once above its output instead of being repeated for every definition it stands for.',
      'A REPL window opens on your file so you can try things against its definitions one line at a time, without changing the file; it offers the same completions and documentation as the editor, including the names your file and your earlier lines defined.',
      'Searching for a function now happens on the documentation page itself, which looks like the rest of Scamper rather than a separate page of its own.',
      'A comment block at the top of your file no longer runs into the documentation on the first function below it, so that function keeps its checked examples and argument checks.',
      'A reactive canvas or container can subscribe to more than one event again, so a program can react to a timer, the mouse, and the keyboard at the same time.',
      'Pictures, charts, and compositions in an embedded reading now appear as themselves rather than as the text of the expression that made them.',
      'A new gradescope library turns your test results into the file Gradescope reads, so an instructor can autograde Scamper work with the same tests you write.'
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
