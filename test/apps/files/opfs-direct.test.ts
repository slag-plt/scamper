import JSZip from 'jszip'
import { beforeEach, describe, expect, test, vi } from 'vitest'
import {
  list,
  openRoot,
  readBlob,
  readBytes,
  remove,
  rename,
  usage,
  zipAll,
} from '../../../src/app/files/opfs-direct'

// The rescue page's own view of OPFS (issue #130), against a fake root that
// records what it was asked to do. That is the point of these specs: what
// matters about this layer is which calls it makes, not what storage does with
// them. Two properties in particular are the reason it exists at all rather
// than reusing src/fs/opfs.ts -- a listing must not read a file's contents,
// and it must not hide anything.

/** The writes the rename fallback sent to the OPFS worker. */
const { writes } = vi.hoisted(() => ({
  writes: [] as { filename: string; bytes: Uint8Array }[],
}))

// The real writer starts a Worker, which jsdom has not got. The fallback path
// is exercised for the calls it makes, not for the bytes landing on disk.
vi.mock('../../../src/fs/opfs-writer', () => ({
  opfsWriter: {
    write: (filename: string, bytes: Uint8Array) => {
      writes.push({ filename, bytes })
      return Promise.resolve()
    },
  },
}))

interface StoredFile {
  contents: string
  lastModified: number
}

class FakeRoot {
  files = new Map<string, StoredFile>()
  directories = new Map<string, FakeRoot>()
  /** Names whose *contents* were read, as opposed to merely stat'd. */
  reads: string[] = []
  /** Every removeEntry call, with the options it was given. */
  removals: { name: string; options?: FileSystemRemoveOptions }[] = []
  /** Every move() call, as [from, to]. */
  moves: [string, string][] = []
  /** Whether a file handle offers move(); Chromium's does, Firefox's not. */
  canMove = true

  /**
   * @returns the file as the browser would, i.e. a real Blob whose lazy reads
   *          are recorded. `getFile()` itself is free -- that is what lets
   *          `list` report a size without touching the bytes.
   */
  private fileOf(name: string): File {
    const stored = this.files.get(name)
    const file = new File([stored?.contents ?? ''], name, {
      lastModified: stored?.lastModified ?? 0,
    })
    const text = File.prototype.text.bind(file)
    const arrayBuffer = File.prototype.arrayBuffer.bind(file)
    return Object.assign(file, {
      text: () => {
        this.reads.push(name)
        return text()
      },
      arrayBuffer: () => {
        this.reads.push(name)
        return arrayBuffer()
      },
    })
  }

  private fileHandle(name: string) {
    const handle = {
      kind: 'file' as const,
      name,
      getFile: () => Promise.resolve(this.fileOf(name)),
    }
    if (!this.canMove) return handle
    return Object.assign(handle, {
      move: (to: string) => {
        this.moves.push([name, to])
        const stored = this.files.get(name)
        if (stored !== undefined) {
          this.files.delete(name)
          this.files.set(to, stored)
        }
        return Promise.resolve()
      },
    })
  }

  getFileHandle(name: string) {
    if (!this.files.has(name)) {
      return Promise.reject(
        new DOMException(`no such file: ${name}`, 'NotFoundError'),
      )
    }
    return Promise.resolve(this.fileHandle(name))
  }

  removeEntry(name: string, options?: FileSystemRemoveOptions) {
    this.removals.push({ name, options })
    this.files.delete(name)
    this.directories.delete(name)
    return Promise.resolve()
  }

  // An async generator is what the real OPFS directory handle hands back, and
  // this stands in for one; there is simply nothing here to await.
  // eslint-disable-next-line @typescript-eslint/require-await
  async *entries() {
    for (const name of this.files.keys()) {
      yield [name, this.fileHandle(name)]
    }
    for (const [name, dir] of this.directories) {
      yield [name, Object.assign(dir, { kind: 'directory' as const, name })]
    }
  }
}

let root: FakeRoot

/** The fake root, as the layer's functions take it. */
function dir(): FileSystemDirectoryHandle {
  return root as unknown as FileSystemDirectoryHandle
}

beforeEach(() => {
  root = new FakeRoot()
  writes.length = 0
})

describe('list', () => {
  test('reports size and time without reading any contents', async () => {
    root.files.set('big.png', { contents: 'x'.repeat(5000), lastModified: 42 })
    root.files.set('hello.scm', { contents: '(display 1)', lastModified: 7 })

    const entries = await list(dir())

    // The whole reason this layer exists: src/fs/opfs.ts's getFileList reads
    // every user file whole to build a five-line preview, which on the storage
    // this page rescues is itself the hazard.
    expect(root.reads).toEqual([])
    const big = entries.find((e) => e.name === 'big.png')
    expect(big?.size).toBe(5000)
    expect(big?.lastModified).toBe(42)
    expect(big?.isDirectory).toBe(false)
    expect(big?.kind).toBe('file')
  })

  test('lists swap files and dotfiles, which the IDE hides', async () => {
    root.files.set('hello.scm', { contents: '', lastModified: 0 })
    root.files.set('hello.scm.crswap', { contents: 'partial', lastModified: 0 })
    root.files.set('.hello.scm.history', { contents: '{}', lastModified: 0 })
    root.files.set('.scamper.config', { contents: '{}', lastModified: 0 })

    expect((await list(dir())).map((e) => e.name).sort()).toEqual([
      '.hello.scm.history',
      '.scamper.config',
      'hello.scm',
      'hello.scm.crswap',
    ])
  })

  test('lists a directory, with no size of its own', async () => {
    root.directories.set('assets', new FakeRoot())

    const entries = await list(dir())
    expect(entries).toEqual([
      {
        name: 'assets',
        kind: 'directory',
        size: 0,
        lastModified: 0,
        isDirectory: true,
      },
    ])
  })
})

describe('remove', () => {
  test('is recursive, so a non-empty directory can go', async () => {
    root.directories.set('assets', new FakeRoot())

    await remove(dir(), 'assets')

    // src/fs/opfs.ts's deleteFile omits this, and so cannot remove one.
    expect(root.removals).toEqual([
      { name: 'assets', options: { recursive: true } },
    ])
  })
})

describe('rename', () => {
  test('prefers move(), which does not read the file', async () => {
    root.files.set('big.png', { contents: 'x'.repeat(5000), lastModified: 0 })

    await rename(dir(), 'big.png', 'bigger.png')

    expect(root.moves).toEqual([['big.png', 'bigger.png']])
    expect(root.reads).toEqual([])
    expect(writes).toEqual([])
    expect(root.removals).toEqual([])
  })

  test('falls back to a copy and a delete where move() is missing', async () => {
    root.canMove = false
    root.files.set('a.scm', { contents: 'contents', lastModified: 0 })

    await rename(dir(), 'a.scm', 'b.scm')

    expect(root.moves).toEqual([])
    expect(writes.map((w) => w.filename)).toEqual(['b.scm'])
    expect(new TextDecoder().decode(writes[0].bytes)).toBe('contents')
    expect(root.removals).toEqual([
      { name: 'a.scm', options: { recursive: true } },
    ])
  })
})

describe('readBlob and readBytes', () => {
  test('read a file back', async () => {
    root.files.set('a.scm', { contents: 'contents', lastModified: 0 })

    expect(await (await readBlob(dir(), 'a.scm')).text()).toBe('contents')
    expect(new TextDecoder().decode(await readBytes(dir(), 'a.scm'))).toBe(
      'contents',
    )
  })
})

describe('zipAll', () => {
  test('archives everything, including what the IDE would filter out', async () => {
    root.files.set('hello.scm', { contents: '(display 1)', lastModified: 0 })
    root.files.set('.hello.scm.history', { contents: '{}', lastModified: 0 })
    root.files.set('hello.scm.crswap', { contents: 'partial', lastModified: 0 })
    const assets = new FakeRoot()
    assets.files.set('logo.png', { contents: 'png', lastModified: 0 })
    root.directories.set('assets', assets)

    const zip = await JSZip.loadAsync(await zipAll(dir()))

    // archive.ts filters with isUserFile, which drops all three of the last
    // ones. This is the copy a student takes before deleting, so it keeps
    // everything.
    expect(Object.keys(zip.files).sort()).toEqual([
      '.hello.scm.history',
      // JSZip records the folder itself as well as what is in it.
      'assets/',
      'assets/logo.png',
      'hello.scm',
      'hello.scm.crswap',
    ])
    expect(await zip.file('hello.scm')?.async('string')).toBe('(display 1)')
  })
})

describe('openRoot and usage', () => {
  /** Stubs `navigator.storage` with `value`, or removes it entirely. */
  function stubStorage(value: unknown) {
    Object.defineProperty(globalThis.navigator, 'storage', {
      value,
      configurable: true,
    })
  }

  test('openRoot hands back the OPFS root', async () => {
    stubStorage({ getDirectory: () => Promise.resolve(root) })
    expect(await openRoot()).toBe(root)
  })

  test('openRoot fails with a sentence where there is no storage', async () => {
    stubStorage(undefined)
    await expect(openRoot()).rejects.toThrow('no private file storage')
  })

  test('usage reports what the browser estimates', async () => {
    stubStorage({ estimate: () => Promise.resolve({ usage: 10, quota: 99 }) })
    expect(await usage()).toEqual({ used: 10, quota: 99 })
  })

  test('usage is null where the browser will not estimate', async () => {
    stubStorage({})
    expect(await usage()).toBeNull()
  })
})
