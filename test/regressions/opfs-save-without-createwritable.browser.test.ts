import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import OPFSFileSystem from '../../src/fs/opfs'

// #429: Safari gained FileSystemFileHandle.createWritable only in version 26,
// and the OPFS backend wrote through it unconditionally. Every save a
// logged-out student made -- each autosave among them -- died with
// "createWritable is not a function", which is what the issue's screenshot
// shows. Chromium has the method, so these tests take it away for their
// duration and ask for the same writes, exercising the path a Safari user is
// left with: a dedicated worker holding a sync access handle.

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

/** The prototype, seen as something `createWritable` can be deleted from. */
const handleProto = FileSystemFileHandle.prototype as unknown as {
  createWritable?: unknown
}
const realCreateWritable = handleProto.createWritable

beforeEach(async () => {
  await clearStorage()
  // Guard the premise: without this the tests pass on the Chromium path and
  // say nothing about the bug.
  expect(typeof realCreateWritable).toBe('function')
  delete handleProto.createWritable
})

afterEach(() => {
  handleProto.createWritable = realCreateWritable
})

describe('OPFS writes in a browser without createWritable', () => {
  test('saves and reads back a file', async () => {
    const fs = await OPFSFileSystem.create()
    await fs.saveFile('hello.scm', '(display "hello")')

    expect(await fs.loadFile('hello.scm')).toBe('(display "hello")')
    expect(await fs.fileExists('hello.scm')).toBe(true)
  })

  test('leaves nothing of a longer previous version behind', async () => {
    const fs = await OPFSFileSystem.create()
    await fs.saveFile('hello.scm', '(display "a long first version")')
    await fs.saveFile('hello.scm', '(+ 1 2)')

    expect(await fs.loadFile('hello.scm')).toBe('(+ 1 2)')
  })

  test('keeps overlapping saves of one file from colliding', async () => {
    // A sync access handle is an exclusive lock on its file, so two writes in
    // flight at once would fail with NoModificationAllowedError if they were
    // not serialised. Autosave plus a history snapshot is exactly this shape.
    const fs = await OPFSFileSystem.create()
    await Promise.all([
      fs.saveFile('hello.scm', '(display 1)'),
      fs.saveFile('hello.scm', '(display 2)'),
      fs.saveFile('other.scm', '(display 3)'),
    ])

    expect(await fs.loadFile('hello.scm')).toMatch(/\(display [12]\)/)
    expect(await fs.loadFile('other.scm')).toBe('(display 3)')
  })

  test('round-trips bytes, so a rename cannot corrupt a file', async () => {
    const fs = await OPFSFileSystem.create()
    const bytes = new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x00, 0xff])
    await fs.saveBytes('image.png', bytes)

    await fs.renameFile('image.png', 'copy.png')
    expect(Array.from(await fs.loadBytes('copy.png'))).toEqual(Array.from(bytes))
    expect(await fs.fileExists('image.png')).toBe(false)
  })

  test('lists what it wrote, with previews', async () => {
    const fs = await OPFSFileSystem.create()
    await fs.saveFile('hello.scm', '(display "hello")\nmore\n')

    const entries = await fs.getFileList()
    expect(entries.map((e) => e.name)).toEqual(['hello.scm'])
    expect(entries[0].preview).toBe('(display "hello")\nmore\n')
  })
})
