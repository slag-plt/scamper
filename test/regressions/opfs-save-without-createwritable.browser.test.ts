import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import OPFSFileSystem from '../../src/fs/opfs'

// #429: Safari gained FileSystemFileHandle.createWritable only in version 26,
// and the OPFS backend wrote through it unconditionally. Every save a
// logged-out student made -- each autosave among them -- died with
// "createWritable is not a function", which is what the issue's screenshot
// shows. Chromium has the method, so these tests take it away for their
// duration and ask for the same writes, exercising the path a Safari user is
// left with: a dedicated worker holding a sync access handle.

/** The prototype, seen as something `createWritable` can be taken off. */
const handleProto = FileSystemFileHandle.prototype as unknown as {
  createWritable?: unknown
}
const realCreateWritable = Object.getOwnPropertyDescriptor(
  handleProto,
  'createWritable',
)

/**
 * Names are unique per test rather than cleared between them: every browser
 * test file shares one origin's storage, so clearing it is a file's way of
 * stepping on its neighbours. (test/fs/opfs.browser.test.ts does clear it,
 * which is why these files run one at a time -- see the browser config.)
 */
let id = 0

function scratch(name: string): string {
  return `safari-${id}-${name}`
}

beforeEach(() => {
  id += 1
  // Guard the premise: without this the tests pass on the Chromium path and
  // say nothing about the bug.
  expect(realCreateWritable).toBeDefined()
  delete handleProto.createWritable
})

afterEach(() => {
  // Restored as it was -- a plain assignment would leave it enumerable, which
  // the real one is not. (`beforeEach` has already insisted it is there.)
  if (realCreateWritable !== undefined) {
    Object.defineProperty(handleProto, 'createWritable', realCreateWritable)
  }
})

describe('OPFS writes in a browser without createWritable', () => {
  test('saves and reads back a file', async () => {
    const fs = await OPFSFileSystem.create()
    const file = scratch('hello.scm')
    await fs.saveFile(file, '(display "hello")')

    expect(await fs.loadFile(file)).toBe('(display "hello")')
    expect(await fs.fileExists(file)).toBe(true)
  })

  test('leaves nothing of a longer previous version behind', async () => {
    const fs = await OPFSFileSystem.create()
    const file = scratch('hello.scm')
    await fs.saveFile(file, '(display "a long first version")')
    await fs.saveFile(file, '(+ 1 2)')

    expect(await fs.loadFile(file)).toBe('(+ 1 2)')
  })

  test('keeps overlapping saves of one file from colliding', async () => {
    // A sync access handle is an exclusive lock on its file, so two writes in
    // flight at once would fail with NoModificationAllowedError if they were
    // not serialised. Autosave plus a history snapshot is exactly this shape.
    const fs = await OPFSFileSystem.create()
    const file = scratch('hello.scm')
    const other = scratch('other.scm')
    await Promise.all([
      fs.saveFile(file, '(display 1)'),
      fs.saveFile(file, '(display 2)'),
      fs.saveFile(other, '(display 3)'),
    ])

    expect(await fs.loadFile(file)).toMatch(/\(display [12]\)/)
    expect(await fs.loadFile(other)).toBe('(display 3)')
  })

  test('round-trips bytes, so a rename cannot corrupt a file', async () => {
    const fs = await OPFSFileSystem.create()
    const from = scratch('image.png')
    const to = scratch('copy.png')
    const bytes = new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x00, 0xff])
    await fs.saveBytes(from, bytes)

    await fs.renameFile(from, to)
    expect(Array.from(await fs.loadBytes(to))).toEqual(Array.from(bytes))
    expect(await fs.fileExists(from)).toBe(false)
  })

  test('lists what it wrote, with a preview', async () => {
    const fs = await OPFSFileSystem.create()
    const file = scratch('hello.scm')
    await fs.saveFile(file, '(display "hello")\nmore\n')

    const entry = (await fs.getFileList()).find((e) => e.name === file)
    expect(entry?.preview).toBe('(display "hello")\nmore\n')
  })
})
