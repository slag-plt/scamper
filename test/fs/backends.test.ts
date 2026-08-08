import { afterEach, describe, expect, test, vi } from 'vitest'
import { getFS, localBackend, serverBackend, setBackend } from '../../src/fs'
import { FlatFileHistory } from '../../src/history/flat-file'
import { ServerHistory } from '../../src/history/server'
import ServerFileSystem from '../../src/fs/server'
import { MockFileSystem } from '../stubs/mock-file-system'
import { initializeBackend, serverSession } from '../../src/app/web/server-session'

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

describe('choosing a backend at startup', () => {
  afterEach(() => {
    vi.unstubAllGlobals()
  })

  test('leaves local storage alone where no server is advertised', async () => {
    const fs = new MockFileSystem()
    setBackend(localBackend(fs))
    // No /config.json is the ordinary case: `npm run dev`, and every
    // deployment until one is configured.
    vi.stubGlobal('fetch', () => Promise.resolve(new Response('', { status: 404 })))

    await initializeBackend()

    expect(getFS()).toBe(fs)
    expect(serverSession()).toBeNull()
  })

  test('uses a server that offers no sign-in, which is the dev stub', async () => {
    setBackend(localBackend(new MockFileSystem()))
    vi.stubGlobal('fetch', (input: string | URL) => {
      const url = input.toString()
      if (url.endsWith('/config.json')) {
        return Promise.resolve(
          new Response(JSON.stringify({ serverUrl: '/api/v1' }), { status: 200 }),
        )
      }
      // SCAMPER_STUB=1: in memory, one namespace, and nothing to sign in to.
      return Promise.resolve(
        new Response(JSON.stringify({ password: false, microsoft: false }), {
          status: 200,
        }),
      )
    })

    await initializeBackend()

    expect(getFS()).toBeInstanceOf(ServerFileSystem)
  })
})
