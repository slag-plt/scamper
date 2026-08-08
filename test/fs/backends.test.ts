import { describe, expect, test, vi } from 'vitest'
import { localBackend, serverBackend } from '../../src/fs'
import { FlatFileHistory } from '../../src/history/flat-file'
import { ServerHistory } from '../../src/history/server'
import ServerFileSystem from '../../src/fs/server'
import { MockFileSystem } from '../stubs/mock-file-system'
import { useServerBackendInDev } from '../../src/app/web/dev-backend'

describe('backend pairing', () => {
  // A file system and its history travel together because the wrong pairing
  // fails silently: a server file system with a flat-file history would write
  // `.{filename}.history` blobs into the storage the server itself provides,
  // which is the layout the database replaces.
  test('a local file system gets the flat-file history', () => {
    const fs = new MockFileSystem()
    const backend = localBackend(fs)
    expect(backend.fs).toBe(fs)
    expect(backend.history).toBeInstanceOf(FlatFileHistory)
  })

  test('a server file system gets the server history', () => {
    const backend = serverBackend('/api/v1')
    expect(backend.fs).toBeInstanceOf(ServerFileSystem)
    expect(backend.history).toBeInstanceOf(ServerHistory)
  })

  test('both halves of a server backend talk to the same API root', async () => {
    const fetchMock = vi.fn(() =>
      Promise.resolve(new Response(JSON.stringify({ files: [] }), { status: 200 })),
    )
    vi.stubGlobal('fetch', fetchMock)
    try {
      const backend = serverBackend('/api/v1')
      await backend.fs.getFileList()
      await backend.history.list()
      const urls = fetchMock.mock.calls.map((call) => String(call[0]))
      expect(urls).toEqual(['/api/v1/fs/files', '/api/v1/history/files'])
    } finally {
      vi.unstubAllGlobals()
    }
  })
})

describe('the development-only server switch', () => {
  // The property that matters is the negative one: nothing but a dev server
  // may move a user's files off local storage. Until BetterAuth lands the
  // server keeps everyone in one unauthorised namespace, so a production build
  // that switched on the mere presence of /config.json would hand every
  // student the same pile of files.
  test('does nothing outside a `--mode server` dev server', async () => {
    expect(SCAMPER_DEV_SERVER).toBe(false)
    const fetchMock = vi.fn(() => Promise.resolve(new Response('{}')))
    vi.stubGlobal('fetch', fetchMock)
    try {
      expect(await useServerBackendInDev()).toBe(false)
      // It does not even look for a config, so there is nothing to go wrong.
      expect(fetchMock).not.toHaveBeenCalled()
    } finally {
      vi.unstubAllGlobals()
    }
  })
})
