import { describe, expect, test } from 'vitest'
import {
  fileKindOf,
  isBinaryName,
  isImageName,
  refuseBinary,
} from '../../src/fs/fs'

// The classification behind #385: a file's name decides how it is read and how
// the editor treats it. These are the cases that decide whether a student's
// file opens, so they are pinned rather than left to the extension table's
// shape.

describe('fileKindOf', () => {
  test('a .scm file is a Scamper program', () => {
    expect(fileKindOf('hw3.scm')).toBe('scamper')
  })

  test('is case-insensitive about the extension', () => {
    // A file that arrives from a Windows machine or a phone camera can be
    // spelled either way, and it is the same kind of file both times.
    expect(fileKindOf('HW3.SCM')).toBe('scamper')
    expect(fileKindOf('CAT.PNG')).toBe('binary')
    expect(fileKindOf('Notes.Md')).toBe('text')
  })

  test('other text extensions are text', () => {
    for (const name of ['notes.md', 'data.csv', 'readme.txt', 'grades.tsv']) {
      expect(fileKindOf(name)).toBe('text')
    }
  })

  test('images, audio, archives and documents are binary', () => {
    for (const name of ['cat.png', 'song.mp3', 'work.zip', 'paper.pdf']) {
      expect(fileKindOf(name)).toBe('binary')
    }
  })

  test('an unknown extension is text, so an unfamiliar file still opens', () => {
    // The safe way round: guessing "binary" for a name we have not thought of
    // would refuse to open a student's file for no reason.
    expect(fileKindOf('notes.rst')).toBe('text')
    expect(fileKindOf('thing.qqq')).toBe('text')
  })

  test('a name with no extension is text', () => {
    expect(fileKindOf('README')).toBe('text')
    expect(fileKindOf('')).toBe('text')
  })

  test('only the last extension counts', () => {
    expect(fileKindOf('archive.tar.gz')).toBe('binary')
    expect(fileKindOf('my.notes.scm')).toBe('scamper')
    // A file's saved history is `.{filename}.history` (#42), which is text.
    expect(fileKindOf('.hello.scm.history')).toBe('text')
  })

  test('a leading dot starts an internal name, not an extension', () => {
    // `.gitignore` is a name beginning with a dot, not a file whose extension
    // is `gitignore`; reading it as one would make every dotfile "text" by
    // accident rather than on purpose.
    expect(fileKindOf('.gitignore')).toBe('text')
  })

  test('.svg is text, being XML a student may want to edit', () => {
    expect(fileKindOf('logo.svg')).toBe('text')
    expect(isImageName('logo.svg')).toBe(false)
  })
})

describe('isImageName', () => {
  test('is true only for images the browser can draw', () => {
    expect(isImageName('cat.png')).toBe(true)
    expect(isImageName('cat.JPEG')).toBe(true)
    expect(isImageName('song.mp3')).toBe(false)
    expect(isImageName('hw3.scm')).toBe(false)
  })

  test('every image is binary, so it never reaches the editor', () => {
    for (const name of ['a.png', 'b.jpg', 'c.gif', 'd.webp']) {
      expect(isBinaryName(name)).toBe(true)
    }
  })
})

describe('refuseBinary', () => {
  test('lets text through', () => {
    expect(() => { refuseBinary('hw3.scm') }).not.toThrow()
    expect(() => { refuseBinary('notes.md') }).not.toThrow()
  })

  test('throws for a binary name, naming the file', () => {
    // The guard that makes the file-destroying bug unwritable: before #385,
    // opening a PNG decoded it as UTF-8 and autosave wrote the result back.
    expect(() => { refuseBinary('cat.png') }).toThrow(/cat\.png/)
  })
})
