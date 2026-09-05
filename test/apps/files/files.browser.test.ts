import { beforeEach, describe, expect, test } from 'vitest'
import { list, openRoot, remove, rename } from '../../../src/app/files/opfs-direct'
import OPFSFileSystem from '../../../src/fs/opfs'

// The rescue page's OPFS layer against real storage in a real browser (issue
// #130). test/apps/files/opfs-direct.test.ts covers the call shapes against a
// fake root; this covers what only Chromium can answer -- above all that a
// `.crswap` file left by an interrupted save is visible here, where the IDE's
// own file system hides it. That file is one of the ways a student gets stuck,
// so this page has to be able to show and delete it.

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

/** Writes `contents` to `name` in the OPFS root. */
async function write(name: string, contents: string) {
  const root = await navigator.storage.getDirectory()
  const handle = await root.getFileHandle(name, { create: true })
  const writable = await handle.createWritable()
  await writable.write(contents)
  await writable.close()
}

beforeEach(async () => {
  await clearStorage()
})

describe('the browser-files page against real storage', () => {
  test('lists a file with its real size, and deletes it', async () => {
    await write('hello.scm', '(display "hello")')
    const root = await openRoot()

    const entry = (await list(root)).find((e) => e.name === 'hello.scm')
    expect(entry?.size).toBe('(display "hello")'.length)
    expect(entry?.lastModified).toBeGreaterThan(0)

    await remove(root, 'hello.scm')
    expect((await list(root)).map((e) => e.name)).not.toContain('hello.scm')
  })

  test('shows the swap file the IDE hides', async () => {
    const root = await openRoot()
    const handle = await root.getFileHandle('hello.scm', { create: true })

    // A write in flight: Chromium puts `hello.scm.crswap` beside the file. A
    // tab that dies here leaves it behind for good.
    const writable = await handle.createWritable()
    await writable.write('(display 1)')
    try {
      expect((await list(root)).map((e) => e.name)).toContain(
        'hello.scm.crswap',
      )
      // The same directory, through the IDE's file system.
      const fs = await OPFSFileSystem.create()
      expect((await fs.getFileList()).map((e) => e.name)).toEqual(['hello.scm'])
    } finally {
      await writable.close()
    }
  })

  test('deletes a directory that has something in it', async () => {
    const root = await openRoot()
    const assets = await root.getDirectoryHandle('assets', { create: true })
    await assets.getFileHandle('logo.png', { create: true })

    // src/fs/opfs.ts's deleteFile omits `{ recursive: true }` and fails here.
    await remove(root, 'assets')
    expect((await list(root)).map((e) => e.name)).not.toContain('assets')
  })

  test('renames a file', async () => {
    await write('a.scm', 'contents')
    const root = await openRoot()

    await rename(root, 'a.scm', 'b.scm')

    const names = (await list(root)).map((e) => e.name)
    expect(names).toContain('b.scm')
    expect(names).not.toContain('a.scm')
    expect(await (await root.getFileHandle('b.scm')).getFile().then((f) => f.text())).toBe('contents')
  })
})
