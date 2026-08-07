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

describe('isHiddenName', () => {
  test('marks dotted names as internal', () => {
    expect(isHiddenName('.scamper.config')).toBe(true)
    expect(isHiddenName('.hello.scm.history')).toBe(true)
    expect(isHiddenName('hello.scm')).toBe(false)
  })
})
