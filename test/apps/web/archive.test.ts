import { describe, expect, test, vi } from 'vitest'
import JSZip from 'jszip'
import { archiveFilename, buildArchive } from '../../../src/app/web/archive'
import { MockFileSystem } from '../../stubs/mock-file-system'

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
    const fs = new MockFileSystem()
    await fs.saveFile('hello.scm', '(display "hello")')
    await fs.saveFile('shapes.scm', '(solid-square 100 "red")')

    expect(await unzip(await buildArchive(fs))).toEqual({
      'hello.scm': '(display "hello")',
      'shapes.scm': '(solid-square 100 "red")',
    })
  })

  test('leaves out internal dotfiles and directories', async () => {
    const fs = new MockFileSystem()
    await fs.saveFile('hello.scm', '(display "hello")')
    await fs.saveFile('.scamper.config', '{}')
    await fs.saveFile('.hello.scm.history', '{"version":1}')
    fs.addDirectory('assets')

    expect(Object.keys(await unzip(await buildArchive(fs)))).toEqual(['hello.scm'])
  })

  test('produces an empty archive when there are no user files', async () => {
    expect(await unzip(await buildArchive(new MockFileSystem()))).toEqual({})
  })

  test('preserves unicode contents through the round trip', async () => {
    const fs = new MockFileSystem()
    await fs.saveFile('notes.scm', '; λ is a lambda — really\n')

    expect((await unzip(await buildArchive(fs)))['notes.scm']).toBe(
      '; λ is a lambda — really\n',
    )
  })

  test('archives a binary file byte for byte', async () => {
    // Reading an image as text would put a broken copy in the archive, which
    // is what a student would find when they unzipped it (#385).
    const png = new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x00, 0xff, 0xfe])
    const fs = new MockFileSystem()
    await fs.saveFile('hw.scm', '(display 1)')
    await fs.saveBytes('cat.png', png)

    const zip = await JSZip.loadAsync(await (await buildArchive(fs)).arrayBuffer())
    expect(await zip.files['cat.png'].async('uint8array')).toEqual(png)
    expect(await zip.files['hw.scm'].async('string')).toBe('(display 1)')
  })

  test('names the unreadable file when a read fails', async () => {
    const fs = new MockFileSystem()
    await fs.saveFile('gone.scm', '(display 1)')
    // The file is listed, but is gone by the time it is read.
    vi.spyOn(fs, 'loadFile').mockRejectedValue(new Error('NotFoundError'))

    await expect(buildArchive(fs)).rejects.toThrow(/gone\.scm/)
  })
})

describe('archiveFilename', () => {
  test('stamps the archive with the local date, zero-padded', () => {
    // Late enough in the day that a UTC-based stamp would name the next one.
    expect(archiveFilename(new Date(2026, 0, 5, 23, 30))).toBe(
      'scamper-files-2026-01-05.zip',
    )
  })
})
