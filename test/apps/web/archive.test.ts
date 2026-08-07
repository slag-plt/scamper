import { describe, expect, test } from 'vitest'
import JSZip from 'jszip'
import { archiveFilename, buildArchive } from '../../../src/app/web/archive'
import type { FS, FileEntry } from '../../../src/fs/fs'

/**
 * An in-memory FS that, unlike MockFileSystem, can report directory entries --
 * the archive needs to skip those.
 */
function mockFS(entries: Record<string, string>, directories: string[] = []): FS {
  const files = new Map(Object.entries(entries))
  const list = (): FileEntry[] => [
    ...[...files.keys()].map((name) => ({ name, preview: null, isDirectory: false })),
    ...directories.map((name) => ({ name, preview: null, isDirectory: true })),
  ]
  return {
    getFileList: () => Promise.resolve(list()),
    fileExists: (n) => Promise.resolve(files.has(n)),
    loadFile: (n) => {
      const contents = files.get(n)
      return contents === undefined
        ? Promise.reject(new Error('NotFoundError'))
        : Promise.resolve(contents)
    },
    saveFile: (n, c) => {
      files.set(n, c)
      return Promise.resolve()
    },
    deleteFile: (n) => {
      files.delete(n)
      return Promise.resolve()
    },
    renameFile: () => Promise.reject(new Error('unused')),
  }
}

/** @returns the archive's entries as a name -> contents map. */
async function unzip(blob: Blob): Promise<Record<string, string>> {
  const zip = await JSZip.loadAsync(await blob.arrayBuffer())
  const contents: Record<string, string> = {}
  await Promise.all(
    Object.values(zip.files).map(async (entry) => {
      contents[entry.name] = await entry.async('string')
    }),
  )
  return contents
}

describe('buildArchive', () => {
  test('archives every user file with its contents', async () => {
    const fs = mockFS({
      'hello.scm': '(display "hello")',
      'shapes.scm': '(solid-square 100 "red")',
    })
    expect(await unzip(await buildArchive(fs))).toEqual({
      'hello.scm': '(display "hello")',
      'shapes.scm': '(solid-square 100 "red")',
    })
  })

  test('leaves out internal dotfiles and directories', async () => {
    const fs = mockFS(
      {
        'hello.scm': '(display "hello")',
        '.scamper.config': '{}',
        '.scamper.lock': '2026-08-07T00:00:00.000Z',
      },
      ['assets'],
    )
    expect(Object.keys(await unzip(await buildArchive(fs)))).toEqual(['hello.scm'])
  })

  test('produces an empty archive when there are no user files', async () => {
    expect(await unzip(await buildArchive(mockFS({})))).toEqual({})
  })

  test('preserves unicode contents through the round trip', async () => {
    const fs = mockFS({ 'notes.scm': '; λ is a lambda — really\n' })
    expect((await unzip(await buildArchive(fs)))['notes.scm']).toBe(
      '; λ is a lambda — really\n',
    )
  })

  test('names the unreadable file when a read fails', async () => {
    const fs = mockFS({ 'gone.scm': '(display 1)' })
    // The file is listed, but is gone by the time it is read.
    fs.loadFile = () => Promise.reject(new Error('NotFoundError'))
    await expect(buildArchive(fs)).rejects.toThrow(/gone\.scm/)
  })
})

describe('archiveFilename', () => {
  test('stamps the archive with the local date', () => {
    expect(archiveFilename(new Date(2026, 7, 7, 13, 45))).toBe(
      'scamper-files-2026-08-07.zip',
    )
  })

  test('zero-pads single-digit months and days', () => {
    expect(archiveFilename(new Date(2026, 0, 5))).toBe('scamper-files-2026-01-05.zip')
  })
})
