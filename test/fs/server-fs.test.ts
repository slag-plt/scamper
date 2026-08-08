import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { ServerFileSystem } from '../../src/fs/server'
import { route } from '../../server/src/api'
import { MemoryFileStore } from '../../server/src/store'
import { MemoryHistoryStore } from '../../server/src/history-store'

const BASE_URL = 'https://files.example/api/v1'

/** One request the client made, as the fake server saw it. */
interface Seen {
  method: string
  path: string
  credentials: RequestCredentials | undefined
}

let store: MemoryFileStore
let history: MemoryHistoryStore

/** The signed-in user the fake server attributes every request to. */
const USER = 'user-1'
let seen: Seen[]

/**
 * Stands the real routing layer up behind `fetch`, so these tests exercise the
 * actual client/server contract -- URLs, methods, encodings, status codes --
 * rather than a hand-written mock that could drift from `server/src/api.ts`.
 */
function installFakeServer(): void {
  vi.stubGlobal('fetch', (input: string | URL, init?: RequestInit) => {
    const url = new URL(input.toString())
    const method = init?.method ?? 'GET'
    seen.push({ method, path: url.pathname, credentials: init?.credentials })

    const body =
      typeof init?.body === 'string'
        ? (JSON.parse(init.body) as unknown)
        : undefined
    return route(
      {
        method,
        path: url.pathname,
        body,
        now: new Date('2026-08-07T14:00:00.000Z'),
        userId: USER,
      },
      { files: store, history },
    ).then(
      (reply) =>
        ({
          ok: reply.status >= 200 && reply.status < 300,
          status: reply.status,
          statusText: '',
          json: () => Promise.resolve(reply.body),
        }) as Response,
    )
  })
}

/** @returns how many requests the client has made so far */
function requestCount(): number {
  return seen.length
}

beforeEach(() => {
  store = new MemoryFileStore()
  history = new MemoryHistoryStore()
  seen = []
  installFakeServer()
})

afterEach(() => {
  vi.unstubAllGlobals()
})

describe('ServerFileSystem', () => {
  test('saves and loads a file', async () => {
    const fs = ServerFileSystem.create(BASE_URL)

    await fs.saveFile('hello.scm', '(+ 1 2)')
    expect(await fs.loadFile('hello.scm')).toBe('(+ 1 2)')
  })

  test('lists what the server holds, previews included', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await fs.saveFile('hello.scm', 'one\ntwo')

    expect(await fs.getFileList()).toEqual([
      { name: 'hello.scm', preview: 'one\ntwo', isDirectory: false },
    ])
  })

  test('deletes a file', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await fs.saveFile('a.scm', 'x')
    await fs.deleteFile('a.scm')

    expect(await fs.getFileList()).toEqual([])
  })

  test('renames in one request rather than copy-then-delete', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await fs.saveFile('old.scm', 'x')
    seen = []

    await fs.renameFile('old.scm', 'new.scm')

    // An interruption partway through a copy-then-delete pair would leave the
    // user with two copies or none, so this must stay a single call.
    expect(requestCount()).toBe(1)
    expect(await fs.loadFile('new.scm')).toBe('x')
  })

  test('a trailing slash on the base URL does not double up', async () => {
    const fs = ServerFileSystem.create(`${BASE_URL}/`)
    await fs.getFileList()

    expect(seen[0].path).toBe('/api/v1/fs/files')
  })

  test('sends credentials so the session cookie rides along', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await fs.getFileList()

    expect(seen[0].credentials).toBe('include')
  })

  test('a name with spaces and parens round-trips', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    const name = 'my program (v2).scm'

    await fs.saveFile(name, 'x')
    expect(await fs.loadFile(name)).toBe('x')
    expect((await fs.getFileList())[0].name).toBe(name)
  })
})

describe('fileExists stays off the network', () => {
  // src/fs/opfs.ts documents fileExists as a hot path: module resolution,
  // import steps, and the `file-exists?` primitive a student can call in a
  // loop. A request per call would make an ordinary loop visibly slow.

  test('a warm cache answers without any request', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await fs.saveFile('a.scm', 'x')
    await fs.fileExists('a.scm')
    seen = []

    for (let i = 0; i < 20; i++) {
      expect(await fs.fileExists('a.scm')).toBe(true)
      expect(await fs.fileExists('nope.scm')).toBe(false)
    }

    expect(requestCount()).toBe(0)
  })

  test('concurrent cold lookups collapse into one request', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await store.write(USER, 'a.scm', 'x')

    const answers = await Promise.all([
      fs.fileExists('a.scm'),
      fs.fileExists('b.scm'),
      fs.fileExists('a.scm'),
    ])

    expect(answers).toEqual([true, false, true])
    expect(requestCount()).toBe(1)
  })

  test('a file saved through this instance is visible immediately', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await fs.fileExists('anything')
    seen = []

    await fs.saveFile('fresh.scm', 'x')

    // A program that writes a file and then asks whether it exists must get
    // the right answer, and must not pay a listing request to find out.
    expect(await fs.fileExists('fresh.scm')).toBe(true)
    expect(requestCount()).toBe(1)
  })

  test('a file deleted through this instance disappears immediately', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await fs.saveFile('a.scm', 'x')
    await fs.fileExists('a.scm')

    await fs.deleteFile('a.scm')
    expect(await fs.fileExists('a.scm')).toBe(false)
  })

  test('a rename moves the name in the cache both ways', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await fs.saveFile('old.scm', 'x')
    await fs.fileExists('old.scm')

    await fs.renameFile('old.scm', 'new.scm')

    expect(await fs.fileExists('old.scm')).toBe(false)
    expect(await fs.fileExists('new.scm')).toBe(true)
  })

  test('another device\'s change shows up at the next listing', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    await fs.fileExists('elsewhere.scm')

    // Something outside this tab writes a file.
    await store.write(USER, 'elsewhere.scm', 'x')

    expect(await fs.fileExists('elsewhere.scm')).toBe(false)
    await fs.getFileList()
    expect(await fs.fileExists('elsewhere.scm')).toBe(true)
  })
})

describe('failures surface rather than corrupt', () => {
  test('a missing file throws instead of returning an error page', async () => {
    const fs = ServerFileSystem.create(BASE_URL)

    await expect(fs.loadFile('ghost.scm')).rejects.toThrow(/404/)
  })

  test('a network failure propagates', async () => {
    vi.stubGlobal('fetch', () => Promise.reject(new Error('offline')))
    const fs = ServerFileSystem.create(BASE_URL)

    await expect(fs.getFileList()).rejects.toThrow('offline')
  })

  test('a malformed listing throws rather than yielding junk entries', async () => {
    vi.stubGlobal('fetch', () =>
      Promise.resolve({
        ok: true,
        status: 200,
        statusText: '',
        json: () => Promise.resolve({ nonsense: true }),
      } as Response),
    )
    const fs = ServerFileSystem.create(BASE_URL)

    await expect(fs.getFileList()).rejects.toThrow(/malformed/)
  })

  test('a failed cold lookup does not leave the cache poisoned', async () => {
    const fs = ServerFileSystem.create(BASE_URL)
    vi.stubGlobal('fetch', () => Promise.reject(new Error('offline')))
    await expect(fs.fileExists('a.scm')).rejects.toThrow('offline')

    // Once the network returns, the next lookup must retry rather than answer
    // from a cache that was never filled.
    await store.write(USER, 'a.scm', 'x')
    installFakeServer()
    expect(await fs.fileExists('a.scm')).toBe(true)
  })
})
