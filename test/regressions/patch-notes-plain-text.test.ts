import { expect, test } from 'vitest'
import { patchNotes } from '../../src/app/web/patch-notes'

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

test('every entry keeps at least one note, and none is empty', () => {
  for (const entry of patchNotes) {
    expect(entry.notes.length, `${entry.version} has no notes`).toBeGreaterThan(0)
    for (const note of entry.notes) {
      expect(note.trim(), `${entry.version} has an empty note`).not.toEqual('')
    }
  }
})
