import { expect, test } from 'vitest'
import { NEXT_RELEASE, patchNotes } from '../../src/app/web/patch-notes'

// The 4.1.1 notes were written with Markdown code spans -- `null` and `pi` --
// but PatchNotesModal.vue renders each note with `{{ item }}`, plain-text
// interpolation with no Markdown pass. Students saw the backticks.
//
// Nothing about writing a note tells you this, and the surrounding notes name
// procedures bare, so the mistake is easy to repeat. These pin the convention.

const allNotes = patchNotes.flatMap((n) => n.notes)

test('no note is written with Markdown that would render literally', () => {
  const offenders = allNotes.filter(
    (note) =>
      note.includes('`') || // code span
      note.includes('**') || // bold
      /\[[^\]]+\]\([^)]+\)/.test(note), // link
  )
  expect(offenders).toEqual([])
})

test('every released entry keeps at least one note, and none is empty', () => {
  for (const entry of patchNotes) {
    // `next` is empty by design between releases: the release pull request
    // renames the accumulated entry to the version it cuts and leaves a fresh
    // empty one behind, which is what stops two later appends from each
    // creating a `next` of their own under .gitattributes' union merge (see
    // docs/releasing.md). A *named* entry with nothing in it is the thing to
    // catch -- a release students are shown nothing for is a forgotten rename.
    if (entry.version !== NEXT_RELEASE) {
      expect(
        entry.notes.length,
        `${entry.version} has no notes`,
      ).toBeGreaterThan(0)
    }
    for (const note of entry.notes) {
      expect(note.trim(), `${entry.version} has an empty note`).not.toEqual('')
    }
  }
})
