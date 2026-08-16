import { beforeEach, describe, expect, test } from 'vitest'
import { API_ROOT, route, type ApiResponse } from '../../server/src/api'
import { MemoryFileStore } from '../../server/src/store'
import { MemoryHistoryStore } from '../../server/src/history-store'

let stores: { files: MemoryFileStore; history: MemoryHistoryStore }

/** Every request in this file is signed in as the same user unless stated. */
const USER = 'user-1'

beforeEach(() => {
  stores = {
    files: new MemoryFileStore(),
    history: new MemoryHistoryStore(),
  }
})

/** Issues one request against the current stores. */
function call(
  method: string,
  path: string,
  body?: unknown,
  now = new Date('2026-08-07T14:00:00.000Z'),
  userId: string | null = USER,
): Promise<ApiResponse> {
  return route({ method, path, body, now, userId }, stores)
}

/** Issues a request against a path below the file-system root. */
function fs(
  method: string,
  suffix: string,
  body?: unknown,
): Promise<ApiResponse> {
  return call(method, `${API_ROOT}/fs${suffix}`, body)
}

describe('api routing', () => {
  test('every route is namespaced by API version', () => {
    // Old front-end releases stay live at their versioned URLs indefinitely
    // (see scripts/deploy), so one server serves many client versions at once
    // and the prefix is what lets a breaking change ship beside the old one.
    expect(API_ROOT).toBe('/api/v1')
  })

  test('health reports ok', async () => {
    expect(await call('GET', `${API_ROOT}/health`)).toEqual({
      status: 200,
      body: { status: 'ok', api: API_ROOT },
    })
  })

  // A server whose database is down answers every request as though nobody
  // were signed in, because reading a session is itself a query. Health has to
  // report that, or the IDE tells a student their session ended and offers a
  // sign-in that cannot work either.
  describe('with storage out of reach', () => {
    let unreachable: typeof stores

    beforeEach(() => {
      unreachable = { ...stores, reachable: () => Promise.resolve(false) }
    })

    test('health reports degraded, so the IDE can go offline', async () => {
      expect(
        await route(
          {
            method: 'GET',
            path: `${API_ROOT}/health`,
            now: new Date(),
            userId: null,
          },
          unreachable,
        ),
      ).toEqual({ status: 503, body: { status: 'degraded', api: API_ROOT } })
    })

    test('a request without a session is 503, not "not signed in"', async () => {
      expect(
        await route(
          {
            method: 'GET',
            path: `${API_ROOT}/fs/files`,
            now: new Date(),
            userId: null,
          },
          unreachable,
        ),
      ).toEqual({ status: 503, body: { error: 'Storage is unavailable' } })
    })
  })

  test('an unclaimed path is a 404', async () => {
    expect((await call('GET', `${API_ROOT}/nope`)).status).toBe(404)
  })

  test('an unversioned path is not served', async () => {
    expect((await call('GET', '/health')).status).toBe(404)
    expect((await call('GET', '/fs/files')).status).toBe(404)
  })

  test('a known path under the wrong method is a 405', async () => {
    expect((await fs('DELETE', '/files')).status).toBe(405)
    expect((await fs('GET', '/rename')).status).toBe(405)
  })
})

describe('file routes', () => {
  test('a fresh store lists nothing', async () => {
    expect(await fs('GET', '/files')).toEqual({ status: 200, body: { files: [] } })
  })

  test('a saved file round-trips', async () => {
    expect((await fs('PUT', '/files/hello.scm', { contents: '(+ 1 2)' })).status).toBe(204)
    expect(await fs('GET', '/files/hello.scm')).toEqual({
      status: 200,
      body: { contents: '(+ 1 2)' },
    })
  })

  test('saving over a file replaces its contents', async () => {
    await fs('PUT', '/files/a.scm', { contents: 'first' })
    await fs('PUT', '/files/a.scm', { contents: 'second' })
    expect((await fs('GET', '/files/a.scm')).body).toEqual({ contents: 'second' })
  })

  test('a save without contents is a 400, not a silent empty file', async () => {
    expect((await fs('PUT', '/files/a.scm', {})).status).toBe(400)
    expect((await fs('PUT', '/files/a.scm', undefined)).status).toBe(400)
    expect((await fs('GET', '/files/a.scm')).status).toBe(404)
  })

  test('reading a missing file is a 404', async () => {
    expect((await fs('GET', '/files/ghost.scm')).status).toBe(404)
  })

  test('deleting removes the file', async () => {
    await fs('PUT', '/files/a.scm', { contents: 'x' })
    expect((await fs('DELETE', '/files/a.scm')).status).toBe(204)
    expect((await fs('GET', '/files/a.scm')).status).toBe(404)
  })

  test('deleting a missing file is a 404', async () => {
    expect((await fs('DELETE', '/files/ghost.scm')).status).toBe(404)
  })

  test('a filename is one encoded segment, so odd names survive', async () => {
    const name = 'my program (v2).scm'
    await fs('PUT', `/files/${encodeURIComponent(name)}`, { contents: 'x' })

    expect((await fs('GET', `/files/${encodeURIComponent(name)}`)).body).toEqual({
      contents: 'x',
    })
    expect((await fs('GET', '/files')).body).toEqual({
      files: [{ name, preview: 'x', isDirectory: false }],
    })
  })
})

describe('rename', () => {
  test('moves contents and frees the old name', async () => {
    await fs('PUT', '/files/old.scm', { contents: 'x' })

    expect((await fs('POST', '/rename', { from: 'old.scm', to: 'new.scm' })).status).toBe(204)
    expect((await fs('GET', '/files/old.scm')).status).toBe(404)
    expect((await fs('GET', '/files/new.scm')).body).toEqual({ contents: 'x' })
  })

  test('overwrites the destination when it already exists', async () => {
    await fs('PUT', '/files/old.scm', { contents: 'keep' })
    await fs('PUT', '/files/new.scm', { contents: 'clobber' })

    expect((await fs('POST', '/rename', { from: 'old.scm', to: 'new.scm' })).status).toBe(204)
    expect((await fs('GET', '/files/new.scm')).body).toEqual({ contents: 'keep' })
  })

  test('renaming a missing file is a 404', async () => {
    expect((await fs('POST', '/rename', { from: 'ghost.scm', to: 'x.scm' })).status).toBe(404)
  })

  test('a rename missing a field is a 400', async () => {
    expect((await fs('POST', '/rename', { from: 'a.scm' })).status).toBe(400)
  })
})

describe('listing', () => {
  test('carries a preview so the client need not read every file', async () => {
    // The client computing previews would cost one request per file; this is
    // the whole reason the listing is a single round trip.
    await fs('PUT', '/files/a.scm', { contents: 'one\ntwo\nthree\nfour\nfive\nsix' })

    expect((await fs('GET', '/files')).body).toEqual({
      files: [
        {
          name: 'a.scm',
          preview: 'one\ntwo\nthree\nfour\nfive',
          isDirectory: false,
        },
      ],
    })
  })

  test('carries no preview for a dotted name, matching the OPFS backend', async () => {
    // A file's history lives beside it as `.{filename}.history` and holds up
    // to fifty whole snapshots. Previewing one would put every past version of
    // every file into a listing that never displays them.
    await fs('PUT', '/files/.hello.scm.history', { contents: 'lots\nand\nlots' })
    await fs('PUT', '/files/hello.scm', { contents: 'shown' })

    expect((await fs('GET', '/files')).body).toEqual({
      files: [
        { name: '.hello.scm.history', preview: null, isDirectory: false },
        { name: 'hello.scm', preview: 'shown', isDirectory: false },
      ],
    })
  })

  test('is sorted by name, matching the OPFS backend', async () => {
    for (const name of ['c.scm', 'a.scm', 'b.scm']) {
      await fs('PUT', `/files/${name}`, { contents: '' })
    }

    const { files } = (await fs('GET', '/files')).body as { files: { name: string }[] }
    expect(files.map((file) => file.name)).toEqual(['a.scm', 'b.scm', 'c.scm'])
  })
})

describe('sessions', () => {
  const NOW = new Date('2026-08-07T14:00:00.000Z')

  /** Issues a request with no session at all. */
  function anonymous(method: string, path: string, body?: unknown) {
    return call(method, path, body, NOW, null)
  }

  test('health answers without a session', async () => {
    // A client asks this before anyone has signed in, so it cannot require one.
    expect((await anonymous('GET', `${API_ROOT}/health`)).status).toBe(200)
  })

  test('every other route is refused without a session', async () => {
    const routes: [string, string][] = [
      ['GET', `${API_ROOT}/fs/files`],
      ['GET', `${API_ROOT}/fs/files/hello.scm`],
      ['PUT', `${API_ROOT}/fs/files/hello.scm`],
      ['DELETE', `${API_ROOT}/fs/files/hello.scm`],
      ['POST', `${API_ROOT}/fs/rename`],
      ['GET', `${API_ROOT}/history/files`],
      ['GET', `${API_ROOT}/history/files/hello.scm`],
      ['GET', `${API_ROOT}/history/files/hello.scm/1`],
      ['POST', `${API_ROOT}/history/files/hello.scm`],
      ['DELETE', `${API_ROOT}/history/files/hello.scm`],
      ['POST', `${API_ROOT}/history/rename`],
    ]

    for (const [method, path] of routes) {
      const reply = await anonymous(method, path, { contents: 'x' })
      // Compared as a labelled string so a failure names the route.
      expect(`${method} ${path} -> ${reply.status.toString()}`).toBe(
        `${method} ${path} -> 401`,
      )
    }
  })

  test("one user cannot see or read another user's files", async () => {
    await call('PUT', `${API_ROOT}/fs/files/hello.scm`, { contents: 'ada' })

    const listed = await call(
      'GET',
      `${API_ROOT}/fs/files`,
      undefined,
      NOW,
      'user-2',
    )
    expect(listed.body).toEqual({ files: [] })

    const read = await call(
      'GET',
      `${API_ROOT}/fs/files/hello.scm`,
      undefined,
      NOW,
      'user-2',
    )
    expect(read.status).toBe(404)
  })

  test("a guessed snapshot id does not reach another user's history", async () => {
    await call('POST', `${API_ROOT}/history/files/hello.scm`, {
      contents: 'ada',
    })
    const { snapshots } = (
      await call('GET', `${API_ROOT}/history/files/hello.scm`)
    ).body as { snapshots: { id: string }[] }
    expect(snapshots).toHaveLength(1)

    // Snapshot ids are sequential, so they are guessable by design; what stops
    // a guess is that reading one is scoped to the asking user's history.
    const stolen = await call(
      'GET',
      `${API_ROOT}/history/files/hello.scm/${snapshots[0].id}`,
      undefined,
      NOW,
      'user-2',
    )
    expect(stolen.status).toBe(404)
  })
})
