import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import { mkdir, mkdtemp, rm, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import path from 'node:path'
import NodeFileSystem from '../../src/fs/node'
import { isHiddenName } from '../../src/fs/fs'

// The file listing computes a preview by reading each file in full. Previews
// are only ever displayed by the file drawer, which hides dotted names, so
// reading those is pure cost -- and it is cost paid on every listing, growing
// with each internal file the IDE keeps (config, lock, per-file history).

let root: string

beforeEach(async () => {
  root = await mkdtemp(path.join(tmpdir(), 'scamper-fs-'))
})

afterEach(async () => {
  await rm(root, { recursive: true, force: true })
})

describe('NodeFileSystem.getFileList', () => {
  test('previews the user\'s files', async () => {
    const fs = await NodeFileSystem.create(root)
    await writeFile(path.join(root, 'hello.scm'), '(display "hello")\n', 'utf-8')

    const [entry] = await fs.getFileList()
    expect(entry.name).toBe('hello.scm')
    expect(entry.preview).toBe('(display "hello")\n'.split('\n').slice(0, 5).join('\n'))
  })

  test('lists internal dotfiles without reading them', async () => {
    const fs = await NodeFileSystem.create(root)
    await writeFile(path.join(root, '.scamper.config'), '{"a":1}', 'utf-8')

    const [entry] = await fs.getFileList()
    // Still listed -- fileExists and the zip export both rely on the listing
    // being complete -- but with no preview, so nothing read it.
    expect(entry.name).toBe('.scamper.config')
    expect(entry.preview).toBeNull()
  })

  test('caps a preview at five lines', async () => {
    const fs = await NodeFileSystem.create(root)
    await writeFile(path.join(root, 'long.scm'), '1\n2\n3\n4\n5\n6\n7\n', 'utf-8')

    const [entry] = await fs.getFileList()
    expect(entry.preview).toBe('1\n2\n3\n4\n5')
  })

  test('reports directories, which have no preview', async () => {
    const fs = await NodeFileSystem.create(root)
    await mkdir(path.join(root, 'assets'))

    const [entry] = await fs.getFileList()
    expect(entry).toEqual({ name: 'assets', preview: null, isDirectory: true })
  })
})

describe('NodeFileSystem.fileExists', () => {
  test('is true for a file and false for a missing one', async () => {
    const fs = await NodeFileSystem.create(root)
    await writeFile(path.join(root, 'hello.scm'), '', 'utf-8')

    expect(await fs.fileExists('hello.scm')).toBe(true)
    expect(await fs.fileExists('nope.scm')).toBe(false)
  })

  test('is true for a directory', async () => {
    // file-exists? is documented to say #t for a directory, so both file
    // systems have to agree on that.
    const fs = await NodeFileSystem.create(root)
    await mkdir(path.join(root, 'assets'))

    expect(await fs.fileExists('assets')).toBe(true)
  })

})

describe('NodeFileSystem bytes (#385)', () => {
  /** A PNG signature: bytes that are not valid UTF-8. */
  const png = new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0xff])

  test('round-trips bytes unchanged', async () => {
    const fs = await NodeFileSystem.create(root)
    await fs.saveBytes('cat.png', png)

    expect(await fs.loadBytes('cat.png')).toEqual(png)
  })

  test('reads a text file as bytes too, since text is UTF-8 bytes', async () => {
    const fs = await NodeFileSystem.create(root)
    await fs.saveFile('hello.scm', '(+ 1 2)')

    expect(new TextDecoder().decode(await fs.loadBytes('hello.scm'))).toBe('(+ 1 2)')
  })

  test('refuses to read or write a binary file as text', async () => {
    // The guard that makes the file-destroying bug unwritable: decoding a PNG
    // as UTF-8 and saving the result back is how a file was lost.
    const fs = await NodeFileSystem.create(root)
    await fs.saveBytes('cat.png', png)

    await expect(fs.loadFile('cat.png')).rejects.toThrow(/cat\.png/)
    await expect(fs.saveFile('cat.png', 'oops')).rejects.toThrow(/cat\.png/)
    // And the file is untouched by the attempt.
    expect(await fs.loadBytes('cat.png')).toEqual(png)
  })

  test('a rename keeps a binary file intact', async () => {
    const fs = await NodeFileSystem.create(root)
    await fs.saveBytes('cat.png', png)
    await fs.renameFile('cat.png', 'kitten.png')

    expect(await fs.loadBytes('kitten.png')).toEqual(png)
    expect(await fs.fileExists('cat.png')).toBe(false)
  })

  test('a binary file carries no preview, so it is never read to build one', async () => {
    const fs = await NodeFileSystem.create(root)
    await fs.saveBytes('cat.png', png)
    await fs.saveFile('hello.scm', 'one\ntwo')

    const entries = await fs.getFileList()
    expect(entries.find((e) => e.name === 'cat.png')?.preview).toBeNull()
    expect(entries.find((e) => e.name === 'hello.scm')?.preview).toBe('one\ntwo')
  })
})

describe('isHiddenName', () => {
  test('marks dotted names as internal', () => {
    expect(isHiddenName('.scamper.config')).toBe(true)
    expect(isHiddenName('.hello.scm.history')).toBe(true)
    expect(isHiddenName('hello.scm')).toBe(false)
  })
})
