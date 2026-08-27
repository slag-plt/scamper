import { beforeEach, describe, expect, test } from 'vitest'
import OPFSFileSystem from '../../src/fs/opfs'

// OPFS itself needs a real browser, so these drive the wrapper against a fake
// root that counts what it was asked to do. That is the point: the properties
// worth pinning here are about which calls the wrapper makes, not about
// storage. `fileExists` in particular runs on hot paths -- module resolution,
// import steps, and the `file-exists?` primitive a student can call in a loop
// -- so it must not enumerate (and thereby read) the whole root.

class FakeRoot {
  files = new Map<string, string>()
  directories = new Set<string>()
  /** How many times the root's contents were enumerated. */
  entriesCalls = 0
  /** Names whose contents were actually read. */
  reads: string[] = []

  private fileHandle(name: string) {
    return {
      kind: 'file',
      name,
      getFile: () => {
        this.reads.push(name)
        return Promise.resolve({
        text: () => Promise.resolve(this.files.get(name) ?? ''),
      })
      },
    }
  }

  getFileHandle(name: string) {
    if (!this.files.has(name)) {
      return Promise.reject(new DOMException(`no such file: ${name}`, 'NotFoundError'))
    }
    return Promise.resolve(this.fileHandle(name))
  }

  getDirectoryHandle(name: string) {
    if (!this.directories.has(name)) {
      return Promise.reject(new DOMException(`no such directory: ${name}`, 'NotFoundError'))
    }
    return Promise.resolve({ kind: 'directory', name })
  }

  // An async generator is what the real OPFS directory handle hands back, and
  // this stands in for one; there is simply nothing here to await.
  // eslint-disable-next-line @typescript-eslint/require-await
  async *entries() {
    this.entriesCalls++
    for (const name of this.files.keys()) {
      yield [name, this.fileHandle(name)]
    }
    for (const name of this.directories) {
      yield [name, { kind: 'directory', name }]
    }
  }
}

let root: FakeRoot

/** @returns an OPFS wrapper backed by the current fake root. */
function mkFS(): Promise<OPFSFileSystem> {
  Object.defineProperty(globalThis.navigator, 'storage', {
    value: { getDirectory: () => Promise.resolve(root) },
    configurable: true,
  })
  return OPFSFileSystem.create()
}

beforeEach(() => {
  root = new FakeRoot()
})

describe('OPFSFileSystem.fileExists', () => {
  test('looks the name up directly instead of enumerating the root', async () => {
    root.files.set('hello.scm', '(display "hello")')
    root.files.set('other.scm', 'x'.repeat(10_000))
    const fs = await mkFS()

    expect(await fs.fileExists('hello.scm')).toBe(true)
    expect(root.entriesCalls).toBe(0)
    // Nothing was read to answer the question.
    expect(root.reads).toEqual([])
  })

  test('is false for a name that is not there', async () => {
    root.files.set('hello.scm', '')
    const fs = await mkFS()

    expect(await fs.fileExists('nope.scm')).toBe(false)
    expect(root.entriesCalls).toBe(0)
  })

  test('is true for a directory', async () => {
    // file-exists? is documented to say #t for a directory, which the old
    // listing-based implementation gave for free. The direct lookup has to
    // fall back to a directory lookup to keep saying so.
    root.directories.add('assets')
    const fs = await mkFS()

    expect(await fs.fileExists('assets')).toBe(true)
  })
})

describe('OPFSFileSystem.getFileList', () => {
  test('reads the user\'s files to preview them, but not internal dotfiles', async () => {
    root.files.set('hello.scm', '(display "hello")')
    root.files.set('.scamper.config', '{}')
    root.files.set('.hello.scm.history', '{"version":1}')
    const fs = await mkFS()

    const entries = await fs.getFileList()
    expect(root.reads).toEqual(['hello.scm'])
    expect(entries.find((e) => e.name === 'hello.scm')?.preview).toBe(
      '(display "hello")',
    )
    // Still listed, just not read.
    expect(entries.find((e) => e.name === '.scamper.config')?.preview).toBeNull()
    expect(entries.map((e) => e.name)).toContain('.hello.scm.history')
  })

  test('sorts directories first, then by name', async () => {
    root.files.set('b.scm', '')
    root.files.set('a.scm', '')
    root.directories.add('assets')
    const fs = await mkFS()

    expect((await fs.getFileList()).map((e) => e.name)).toEqual([
      'assets',
      'a.scm',
      'b.scm',
    ])
  })
})
