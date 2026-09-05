import { describe as suite, expect, test } from 'vitest'
import {
  describe,
  formatSize,
  formatWhen,
  sortEntries,
  totalSize,
} from '../../../src/app/files/entries'
import type { StorageEntry } from '../../../src/app/files/opfs-direct'

// The rescue page's presentation layer (issue #130). It is split out from the
// component precisely so it can be tested like this: values in, strings out,
// no DOM and no fakes.

/** A file entry, with only what a spec cares about spelled out. */
function file(name: string, size = 0, lastModified = 0): StorageEntry {
  return { name, kind: 'file', size, lastModified, isDirectory: false }
}

function folder(name: string): StorageEntry {
  return { name, kind: 'directory', size: 0, lastModified: 0, isDirectory: true }
}

suite('describe', () => {
  test('names the file a history belongs to', () => {
    // The point of the label: `.hello.scm.history` alone tells a student
    // nothing about which of their files it would take with it.
    expect(describe(file('.hello.scm.history'))).toBe(
      'Save history for hello.scm',
    )
  })

  test('explains a leftover swap file', () => {
    expect(describe(file('hello.scm.crswap'))).toBe(
      'Left over from an interrupted save of hello.scm',
    )
  })

  test('explains an old settings file', () => {
    expect(describe(file('.scamper.config'))).toBe(
      'Settings file left by an older version of Scamper',
    )
  })

  test('labels ordinary files by kind', () => {
    expect(describe(file('hello.scm'))).toBe('Scamper program')
    expect(describe(file('cat.png'))).toBe('Image')
    expect(describe(file('song.mp3'))).toBe('Data file')
    expect(describe(file('notes.txt'))).toBe('Text file')
    expect(describe(folder('assets'))).toBe('Folder')
  })
})

suite('formatSize', () => {
  test('scales to a unit worth reading', () => {
    expect(formatSize(0)).toBe('0 B')
    expect(formatSize(999)).toBe('999 B')
    expect(formatSize(1024)).toBe('1.0 KB')
    expect(formatSize(1536)).toBe('1.5 KB')
    expect(formatSize(5 * 1024 * 1024)).toBe('5.0 MB')
    expect(formatSize(212 * 1024 * 1024)).toBe('212 MB')
    expect(formatSize(2.1 * 1024 * 1024 * 1024)).toBe('2.1 GB')
  })

  test('says so rather than lying about a size it has not got', () => {
    expect(formatSize(-1)).toBe('—')
    expect(formatSize(NaN)).toBe('—')
  })
})

suite('formatWhen', () => {
  test('is empty when the browser did not say', () => {
    expect(formatWhen(0)).toBe('')
  })

  test('renders a real time', () => {
    expect(formatWhen(Date.UTC(2026, 0, 2, 3, 4))).not.toBe('')
  })
})

suite('sortEntries', () => {
  test('puts directories first, then names', () => {
    const entries = [file('b.scm'), file('a.scm'), folder('assets')]

    expect(sortEntries(entries).map((e) => e.name)).toEqual([
      'assets',
      'a.scm',
      'b.scm',
    ])
    // The input is left alone: this page re-renders from what it was given.
    expect(entries.map((e) => e.name)).toEqual(['b.scm', 'a.scm', 'assets'])
  })
})

suite('totalSize', () => {
  test('adds the sizes up', () => {
    expect(totalSize([file('a', 10), file('b', 32), folder('assets')])).toBe(42)
  })

  test('is zero for nothing', () => {
    expect(totalSize([])).toBe(0)
  })
})
