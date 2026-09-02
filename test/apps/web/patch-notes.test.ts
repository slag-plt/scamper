import { describe, expect, test } from 'vitest'
import { flushPromises, mount } from '@vue/test-utils'
import PatchNotesModal from '../../../src/app/web/components/PatchNotesModal.vue'
import {
  compareVersions,
  NEXT_RELEASE,
  patchNotes,
  patchNotesSince,
  type PatchNote,
} from '../../../src/app/web/patch-notes'

const released = () => patchNotes.filter((n) => n.version !== NEXT_RELEASE)

describe('compareVersions', () => {
  test('orders dotted numeric versions', () => {
    expect(Math.sign(compareVersions('3.5.0', '3.4.0'))).toBe(1)
    expect(Math.sign(compareVersions('3.4.0', '3.5.0'))).toBe(-1)
    expect(compareVersions('3.5.0', '3.5.0')).toBe(0)
  })

  test('compares numerically, not lexically', () => {
    expect(Math.sign(compareVersions('3.10.0', '3.9.0'))).toBe(1)
  })

  test('treats missing trailing components as zero', () => {
    expect(compareVersions('3.5', '3.5.0')).toBe(0)
    expect(Math.sign(compareVersions('3.5.1', '3.5'))).toBe(1)
  })
})

describe('patchNotesSince', () => {
  test('every entry is a released version or the accumulating `next`', () => {
    for (const note of released()) {
      expect(note.version).toMatch(/^\d+(\.\d+)*$/)
      // A released entry says something; only `next` may be empty, and only
      // until the first note lands under it.
      expect(note.notes.length).toBeGreaterThan(0)
    }
  })

  test('there is exactly one `next`, so notes have one place to go', () => {
    // Zero means a release renamed it and left nothing behind; two means two
    // pull requests each created one and the union merge kept both.
    expect(patchNotes.filter((n) => n.version === NEXT_RELEASE)).toHaveLength(1)
  })

  test('never returns `next`, whose release has not been named', () => {
    expect(patchNotesSince('0.0.0', '999.0.0').map((n) => n.version)).not.toContain(
      NEXT_RELEASE,
    )
  })

  test('excludes versions at or below lastSeen and above current', () => {
    // Already caught up to the newest release: nothing to show.
    const newest = released()
      .map((n) => n.version)
      .sort((a, b) => compareVersions(b, a))[0]
    expect(patchNotesSince(newest, newest)).toEqual([])
    // current below every note: nothing to show.
    expect(patchNotesSince('0.0.0', '0.0.1')).toEqual([])
  })

  test('returns every released entry after lastSeen up to current, newest-first', () => {
    const all = patchNotesSince('0.0.0', '999.0.0')
    expect(all.length).toBe(released().length)
    for (let i = 1; i < all.length; i++) {
      // strictly descending
      expect(compareVersions(all[i - 1].version, all[i].version)).toBeGreaterThan(0)
    }
    for (const note of all) {
      expect(compareVersions(note.version, '0.0.0')).toBeGreaterThan(0)
    }
  })

  test('the upper bound is inclusive (a note equal to current is shown)', () => {
    const newest = patchNotesSince('0.0.0', '999.0.0')[0]
    expect(patchNotesSince('0.0.0', newest.version)[0].version).toBe(
      newest.version,
    )
  })
})

describe('PatchNotesModal', () => {
  const notes: PatchNote[] = [
    { version: '3.5.0', title: 'Editor upgrades', notes: ['Alpha', 'Beta'] },
    { version: '3.4.0', notes: ['Gamma'] },
  ]

  test('renders each version, title, and bullet', () => {
    const wrapper = mount(PatchNotesModal, { props: { open: true, notes } })
    const text = wrapper.text()
    expect(text).toContain('Version 3.5.0')
    expect(text).toContain('Editor upgrades')
    expect(text).toContain('Alpha')
    expect(text).toContain('Beta')
    expect(text).toContain('Version 3.4.0')
    expect(text).toContain('Gamma')
    expect(wrapper.findAll('li')).toHaveLength(3)
    wrapper.unmount()
  })

  test('emits close when the "Got it" button is clicked', async () => {
    const wrapper = mount(PatchNotesModal, { props: { open: true, notes } })
    await wrapper.find('.patch-notes__button').trigger('click')
    expect(wrapper.emitted('close')).toHaveLength(1)
    wrapper.unmount()
  })

  test('emits close when the dialog is dismissed (Esc/backdrop)', async () => {
    const wrapper = mount(PatchNotesModal, { props: { open: true, notes } })
    wrapper.find('dialog').element.dispatchEvent(new Event('close'))
    await flushPromises()
    expect(wrapper.emitted('close')).toHaveLength(1)
    wrapper.unmount()
  })
})
