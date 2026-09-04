import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { ServerHistory } from '../../src/history/server'
import { ServerUnreachableError } from '../../src/fs/unreachable'
import { MERGE_WINDOW_MS, MAX_SNAPSHOTS } from '../../src/history/policy'
import { route } from '../../server/src/api'
import { MemoryFileStore } from '../../server/src/store'
import { MemoryHistoryStore } from '../../server/src/history-store'

// The server-backed history, driven against the real routing layer through a
// stubbed fetch, so these check the actual contract -- paths, encodings, status
// codes -- rather than a mock that could drift from server/src/api.ts.
//
// What is worth pinning here is the traffic. A history holds up to fifty copies
// of a file, and the reason it moved out of a flat blob is so that listing and
// browsing it stop moving those copies around.

const BASE_URL = 'https://files.example/api/v1'
const START = new Date('2026-08-07T14:00:00.000Z')

/** @returns `START` advanced by `ms`. */
function at(ms: number): Date {
  return new Date(START.getTime() + ms)
}

let stores: { files: MemoryFileStore; history: MemoryHistoryStore }

/** The signed-in user the fake server attributes every request to. */
const USER = 'user-1'
let seen: { method: string; path: string }[]
/** What the fake server's clock reads; the server stamps snapshots with it. */
let serverNow: Date

function installFakeServer(): void {
  vi.stubGlobal('fetch', (input: string | URL, init?: RequestInit) => {
    const url = new URL(input.toString())
    const method = init?.method ?? 'GET'
    seen.push({ method, path: url.pathname })

    const body =
      typeof init?.body === 'string'
        ? (JSON.parse(init.body) as unknown)
        : undefined
    return route(
      { method, path: url.pathname, body, now: serverNow, userId: USER },
      stores,
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

/** Bytes of file contents the client has pulled down, across all replies. */
function requestCount(): number {
  return seen.length
}

beforeEach(() => {
  stores = {
    files: new MemoryFileStore(),
    history: new MemoryHistoryStore(),
  }
  seen = []
  serverNow = START
  installFakeServer()
})

afterEach(() => {
  vi.unstubAllGlobals()
})

/** Records a save at the given server time. */
async function record(
  history: ServerHistory,
  filename: string,
  contents: string,
  now: Date,
  options?: Parameters<ServerHistory['record']>[3],
) {
  serverNow = now
  return await history.record(filename, contents, now, options)
}

describe('recording', () => {
  test('records the first save', async () => {
    const history = ServerHistory.create(BASE_URL)
    const result = await record(history, 'hello.scm', '(display 1)', START)

    expect(result.recorded).toBe(true)
    expect(result.head?.contents).toBe('(display 1)')
    expect(result.head?.time).toBe(START.toISOString())
  })

  test('ignores a save that changed nothing', async () => {
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'hello.scm', 'same', START)
    const result = await record(
      history,
      'hello.scm',
      'same',
      at(10 * MERGE_WINDOW_MS),
    )

    expect(result.recorded).toBe(false)
  })

  test('folds edits inside the merge window into the open snapshot', async () => {
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'hello.scm', 'first', START)
    const result = await record(
      history,
      'hello.scm',
      'second',
      at(MERGE_WINDOW_MS - 1),
    )

    expect(result.recorded).toBe(false)
    expect((await history.index('hello.scm')).snapshots).toHaveLength(1)
  })

  test('opens a new snapshot once the window has passed', async () => {
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'hello.scm', 'first', START)
    await record(history, 'hello.scm', 'second', at(MERGE_WINDOW_MS))

    const { snapshots } = await history.index('hello.scm')
    expect(snapshots).toHaveLength(2)
    expect(await history.read('hello.scm', snapshots[0].id)).toBe('second')
    expect(await history.read('hello.scm', snapshots[1].id)).toBe('first')
  })

  test('forcing records inside the window', async () => {
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'hello.scm', 'first', START)
    const result = await record(history, 'hello.scm', 'second', at(1_000), {
      force: true,
    })

    expect(result.recorded).toBe(true)
  })

  test('drops the oldest past the cap', async () => {
    const history = ServerHistory.create(BASE_URL)
    for (let i = 0; i <= MAX_SNAPSHOTS; i++) {
      await record(history, 'hello.scm', `edit ${i.toString()}`, at(i * MERGE_WINDOW_MS))
    }

    const { snapshots } = await history.index('hello.scm')
    expect(snapshots).toHaveLength(MAX_SNAPSHOTS)
    expect(await history.read('hello.scm', snapshots[0].id)).toBe(
      `edit ${MAX_SNAPSHOTS.toString()}`,
    )
    // 'edit 0' fell off the end; the oldest kept is the one after it.
    expect(await history.read('hello.scm', snapshots[snapshots.length - 1].id)).toBe(
      'edit 1',
    )
  })

  test('keeps no history of internal files', async () => {
    const history = ServerHistory.create(BASE_URL)
    const result = await record(history, '.scamper.config', '{}', START)

    expect(result.recorded).toBe(false)
    expect((await history.index('.scamper.config')).snapshots).toEqual([])
  })

  test('the server stamps the time, not the client', async () => {
    // A history spans a student's machines. If a laptop running ten minutes
    // fast set the timestamps, its snapshots would sort above ones taken later
    // elsewhere, and the timeline would match neither machine.
    const history = ServerHistory.create(BASE_URL)
    serverNow = START
    const clientClock = new Date(START.getTime() + 10 * 60_000)

    const result = await history.record('hello.scm', 'x', clientClock)

    expect(result.head?.time).toBe(START.toISOString())
  })
})

describe('the cached head keeps autosave off the network', () => {
  test('an unchanged save makes no request at all', async () => {
    const history = ServerHistory.create(BASE_URL)
    const { head } = await record(history, 'hello.scm', 'same', START)
    seen = []

    const result = await history.record('hello.scm', 'same', at(3_000), {
      knownHead: head,
    })

    expect(result.recorded).toBe(false)
    expect(requestCount()).toBe(0)
  })

  test('an edit inside the merge window also makes no request', async () => {
    // Autosave fires every few seconds while a student types; without this the
    // server would see a request every tick for the whole minute.
    const history = ServerHistory.create(BASE_URL)
    const { head } = await record(history, 'hello.scm', 'first', START)
    seen = []

    const result = await history.record('hello.scm', 'second', at(1_000), {
      knownHead: head,
    })

    expect(result.recorded).toBe(false)
    expect(requestCount()).toBe(0)
  })

  test('a save the cache cannot rule out does reach the server', async () => {
    const history = ServerHistory.create(BASE_URL)
    const { head } = await record(history, 'hello.scm', 'first', START)
    seen = []

    serverNow = at(MERGE_WINDOW_MS)
    const result = await history.record(
      'hello.scm',
      'second',
      at(MERGE_WINDOW_MS),
      { knownHead: head },
    )

    expect(result.recorded).toBe(true)
    expect(requestCount()).toBe(1)
  })
})

describe('browsing moves no more than it must', () => {
  test('listing carries names and deletion marks, never contents', async () => {
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'hello.scm', 'a'.repeat(5_000), START)
    await record(history, 'gone.scm', 'b'.repeat(5_000), START)
    serverNow = at(1_000)
    await history.markDeleted('gone.scm')

    expect(await history.list()).toEqual([
      { filename: 'gone.scm', deletedAt: at(1_000).toISOString() },
      { filename: 'hello.scm' },
    ])
  })

  test('the index carries times, never contents', async () => {
    const history = ServerHistory.create(BASE_URL)
    for (let i = 0; i < 5; i++) {
      await record(history, 'hello.scm', `edit ${i.toString()}`, at(i * MERGE_WINDOW_MS))
    }

    const { snapshots } = await history.index('hello.scm')
    expect(snapshots).toHaveLength(5)
    for (const snapshot of snapshots) {
      expect(Object.keys(snapshot).sort()).toEqual(['id', 'time'])
    }
  })

  test('one version is fetched at a time, by id', async () => {
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'hello.scm', 'first', START)
    await record(history, 'hello.scm', 'second', at(MERGE_WINDOW_MS))

    const { snapshots } = await history.index('hello.scm')
    seen = []

    expect(await history.read('hello.scm', snapshots[1].id)).toBe('first')
    expect(requestCount()).toBe(1)
  })

  test('a snapshot that aged out reads as null rather than throwing', async () => {
    // The list a version was picked from is always a moment old, so this is an
    // ordinary outcome the browser has to render, not a failure.
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'hello.scm', 'x', START)

    expect(await history.read('hello.scm', '9999')).toBeNull()
  })
})

describe('renaming and deleting', () => {
  test('a rename carries the history to the new name', async () => {
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'old.scm', 'x', START)
    await history.rename('old.scm', 'new.scm')

    expect((await history.index('old.scm')).snapshots).toEqual([])
    const { snapshots } = await history.index('new.scm')
    expect(await history.read('new.scm', snapshots[0].id)).toBe('x')
  })

  test('renaming a file with no history is not an error', async () => {
    const history = ServerHistory.create(BASE_URL)
    await expect(history.rename('ghost.scm', 'new.scm')).resolves.toBeUndefined()
  })

  test('a deleted file keeps its history, marked', async () => {
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'hello.scm', 'x', START)
    serverNow = at(1_000)
    await history.markDeleted('hello.scm')

    const index = await history.index('hello.scm')
    expect(index.deletedAt).toBe(at(1_000).toISOString())
    expect(index.snapshots).toHaveLength(1)
  })

  test('a later save clears the mark, even when it adds no snapshot', async () => {
    // Recreating a deleted file with its original contents: the save itself
    // adds nothing, but the history is no longer of a deleted file.
    const history = ServerHistory.create(BASE_URL)
    await record(history, 'hello.scm', 'x', START)
    serverNow = at(1_000)
    await history.markDeleted('hello.scm')

    const result = await record(history, 'hello.scm', 'x', at(2_000))

    expect(result.recorded).toBe(false)
    expect((await history.index('hello.scm')).deletedAt).toBeUndefined()
  })

  test('a file with no history leaves none behind when deleted', async () => {
    const history = ServerHistory.create(BASE_URL)
    serverNow = at(1_000)
    await history.markDeleted('hello.scm')

    expect(await history.list()).toEqual([])
  })
})

describe('failures surface rather than corrupt', () => {
  // The same conversion the file system does, for the same reason: the IDE
  // treats an unreachable server as an offline state, not a fault (#357).
  test('an unreachable server raises ServerUnreachableError', async () => {
    vi.stubGlobal('fetch', () =>
      Promise.reject(new TypeError('Failed to fetch')),
    )
    const history = ServerHistory.create(BASE_URL)

    await expect(history.list()).rejects.toThrow(ServerUnreachableError)
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
    const history = ServerHistory.create(BASE_URL)

    await expect(history.list()).rejects.toThrow(/malformed/)
  })

  test('a name with spaces and parens round-trips', async () => {
    const history = ServerHistory.create(BASE_URL)
    const name = 'my program (v2).scm'
    await record(history, name, 'x', START)

    const { snapshots } = await history.index(name)
    expect(await history.read(name, snapshots[0].id)).toBe('x')
    expect(await history.list()).toEqual([{ filename: name }])
  })

  test('a trailing slash on the base URL does not double up', async () => {
    const history = ServerHistory.create(`${BASE_URL}/`)
    await history.list()

    expect(seen[0].path).toBe('/api/v1/history/files')
  })
})
