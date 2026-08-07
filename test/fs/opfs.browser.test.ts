import { beforeEach, describe, expect, test } from 'vitest'
import OPFSFileSystem from '../../src/fs/opfs'

// The OPFS wrapper against real storage in a real browser. test/fs/opfs.test.ts
// covers the call shapes against a fake root; these cover the things only
// Chromium can answer -- notably that a write leaves a `.crswap` sibling in the
// directory, which the rest of Scamper must never be shown.

/** Empties the origin's storage, which persists between tests in one browser. */
async function clearStorage() {
  const root = await navigator.storage.getDirectory()
  for await (const name of (root as unknown as { keys: () => AsyncIterable<string> }).keys()) {
    try {
      await root.removeEntry(name, { recursive: true })
    } catch {
      // A swap file for a write that has since closed may already be gone.
    }
  }
}

beforeEach(async () => {
  await clearStorage()
})

describe('OPFSFileSystem against real storage', () => {
  test('round-trips a file', async () => {
    const fs = await OPFSFileSystem.create()
    await fs.saveFile('hello.scm', '(display "hello")')

    expect(await fs.loadFile('hello.scm')).toBe('(display "hello")')
    expect(await fs.fileExists('hello.scm')).toBe(true)
    expect(await fs.fileExists('nope.scm')).toBe(false)
  })

  test('says a directory exists', async () => {
    // getFileHandle throws TypeMismatchError for a directory, so fileExists
    // has to fall back to a directory lookup. `file-exists?` is documented to
    // answer #t here, and the old listing-based implementation did.
    const fs = await OPFSFileSystem.create()
    const root = await navigator.storage.getDirectory()
    await root.getDirectoryHandle('assets', { create: true })

    expect(await fs.fileExists('assets')).toBe(true)
  })

  test('hides the swap file backing an open write', async () => {
    const fs = await OPFSFileSystem.create()
    const root = await navigator.storage.getDirectory()
    const handle = await root.getFileHandle('hello.scm', { create: true })

    // A write in flight: Chromium puts `hello.scm.crswap` beside the file.
    const writable = await handle.createWritable()
    await writable.write('(display 1)')
    try {
      const raw: string[] = []
      for await (const name of (root as unknown as {
        keys: () => AsyncIterable<string>
      }).keys()) {
        raw.push(name)
      }
      // Guard the premise: if Chromium stops doing this, this test should say
      // so rather than quietly passing for the wrong reason.
      expect(raw).toContain('hello.scm.crswap')

      expect((await fs.getFileList()).map((e) => e.name)).toEqual(['hello.scm'])
    } finally {
      await writable.close()
    }
  })

  test('previews user files but not internal dotfiles', async () => {
    const fs = await OPFSFileSystem.create()
    await fs.saveFile('hello.scm', '(display "hello")\nmore\n')
    await fs.saveFile('.scamper.config', '{}')

    const entries = await fs.getFileList()
    expect(entries.find((e) => e.name === 'hello.scm')?.preview).toBe(
      '(display "hello")\nmore\n',
    )
    expect(entries.find((e) => e.name === '.scamper.config')?.preview).toBeNull()
  })

  test('renames and deletes', async () => {
    const fs = await OPFSFileSystem.create()
    await fs.saveFile('a.scm', 'contents')

    await fs.renameFile('a.scm', 'b.scm')
    expect(await fs.fileExists('a.scm')).toBe(false)
    expect(await fs.loadFile('b.scm')).toBe('contents')

    await fs.deleteFile('b.scm')
    expect(await fs.fileExists('b.scm')).toBe(false)
  })
})
