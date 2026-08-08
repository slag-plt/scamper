import { beforeEach, describe, expect, test } from 'vitest'
import { API_ROOT, route, type ApiResponse } from '../../server/src/api'
import { FileStore } from '../../server/src/store'
import { HistoryStore } from '../../server/src/history-store'

let stores: { files: FileStore; history: HistoryStore }

beforeEach(() => {
  stores = { files: new FileStore(), history: new HistoryStore() }
})

/** Issues one request against the current stores. */
function call(
  method: string,
  path: string,
  body?: unknown,
  now = new Date('2026-08-07T14:00:00.000Z'),
): ApiResponse {
  return route({ method, path, body, now }, stores)
}

/** Issues a request against a path below the file-system root. */
function fs(method: string, suffix: string, body?: unknown): ApiResponse {
  return call(method, `${API_ROOT}/fs${suffix}`, body)
}

describe('api routing', () => {
  test('every route is namespaced by API version', () => {
    // Old front-end releases stay live at their versioned URLs indefinitely
    // (see scripts/deploy), so one server serves many client versions at once
    // and the prefix is what lets a breaking change ship beside the old one.
    expect(API_ROOT).toBe('/api/v1')
  })

  test('health reports ok', () => {
    expect(call('GET', `${API_ROOT}/health`)).toEqual({
      status: 200,
      body: { status: 'ok', api: API_ROOT },
    })
  })

  test('an unclaimed path is a 404', () => {
    expect(call('GET', `${API_ROOT}/nope`).status).toBe(404)
  })

  test('an unversioned path is not served', () => {
    expect(call('GET', '/health').status).toBe(404)
    expect(call('GET', '/fs/files').status).toBe(404)
  })

  test('a known path under the wrong method is a 405', () => {
    expect(fs('DELETE', '/files').status).toBe(405)
    expect(fs('GET', '/rename').status).toBe(405)
  })
})

describe('file routes', () => {
  test('a fresh store lists nothing', () => {
    expect(fs('GET', '/files')).toEqual({ status: 200, body: { files: [] } })
  })

  test('a saved file round-trips', () => {
    expect(fs('PUT', '/files/hello.scm', { contents: '(+ 1 2)' }).status).toBe(204)
    expect(fs('GET', '/files/hello.scm')).toEqual({
      status: 200,
      body: { contents: '(+ 1 2)' },
    })
  })

  test('saving over a file replaces its contents', () => {
    fs('PUT', '/files/a.scm', { contents: 'first' })
    fs('PUT', '/files/a.scm', { contents: 'second' })
    expect(fs('GET', '/files/a.scm').body).toEqual({ contents: 'second' })
  })

  test('a save without contents is a 400, not a silent empty file', () => {
    expect(fs('PUT', '/files/a.scm', {}).status).toBe(400)
    expect(fs('PUT', '/files/a.scm', undefined).status).toBe(400)
    expect(fs('GET', '/files/a.scm').status).toBe(404)
  })

  test('reading a missing file is a 404', () => {
    expect(fs('GET', '/files/ghost.scm').status).toBe(404)
  })

  test('deleting removes the file', () => {
    fs('PUT', '/files/a.scm', { contents: 'x' })
    expect(fs('DELETE', '/files/a.scm').status).toBe(204)
    expect(fs('GET', '/files/a.scm').status).toBe(404)
  })

  test('deleting a missing file is a 404', () => {
    expect(fs('DELETE', '/files/ghost.scm').status).toBe(404)
  })

  test('a filename is one encoded segment, so odd names survive', () => {
    const name = 'my program (v2).scm'
    fs('PUT', `/files/${encodeURIComponent(name)}`, { contents: 'x' })

    expect(fs('GET', `/files/${encodeURIComponent(name)}`).body).toEqual({
      contents: 'x',
    })
    expect(fs('GET', '/files').body).toEqual({
      files: [{ name, preview: 'x', isDirectory: false }],
    })
  })
})

describe('rename', () => {
  test('moves contents and frees the old name', () => {
    fs('PUT', '/files/old.scm', { contents: 'x' })

    expect(fs('POST', '/rename', { from: 'old.scm', to: 'new.scm' }).status).toBe(204)
    expect(fs('GET', '/files/old.scm').status).toBe(404)
    expect(fs('GET', '/files/new.scm').body).toEqual({ contents: 'x' })
  })

  test('overwrites the destination when it already exists', () => {
    fs('PUT', '/files/old.scm', { contents: 'keep' })
    fs('PUT', '/files/new.scm', { contents: 'clobber' })

    expect(fs('POST', '/rename', { from: 'old.scm', to: 'new.scm' }).status).toBe(204)
    expect(fs('GET', '/files/new.scm').body).toEqual({ contents: 'keep' })
  })

  test('renaming a missing file is a 404', () => {
    expect(fs('POST', '/rename', { from: 'ghost.scm', to: 'x.scm' }).status).toBe(404)
  })

  test('a rename missing a field is a 400', () => {
    expect(fs('POST', '/rename', { from: 'a.scm' }).status).toBe(400)
  })
})

describe('listing', () => {
  test('carries a preview so the client need not read every file', () => {
    // The client computing previews would cost one request per file; this is
    // the whole reason the listing is a single round trip.
    fs('PUT', '/files/a.scm', { contents: 'one\ntwo\nthree\nfour\nfive\nsix' })

    expect(fs('GET', '/files').body).toEqual({
      files: [
        {
          name: 'a.scm',
          preview: 'one\ntwo\nthree\nfour\nfive',
          isDirectory: false,
        },
      ],
    })
  })

  test('carries no preview for a dotted name, matching the OPFS backend', () => {
    // A file's history lives beside it as `.{filename}.history` and holds up
    // to fifty whole snapshots. Previewing one would put every past version of
    // every file into a listing that never displays them.
    fs('PUT', '/files/.hello.scm.history', { contents: 'lots\nand\nlots' })
    fs('PUT', '/files/hello.scm', { contents: 'shown' })

    expect(fs('GET', '/files').body).toEqual({
      files: [
        { name: '.hello.scm.history', preview: null, isDirectory: false },
        { name: 'hello.scm', preview: 'shown', isDirectory: false },
      ],
    })
  })

  test('is sorted by name, matching the OPFS backend', () => {
    for (const name of ['c.scm', 'a.scm', 'b.scm']) {
      fs('PUT', `/files/${name}`, { contents: '' })
    }

    const { files } = fs('GET', '/files').body as { files: { name: string }[] }
    expect(files.map((file) => file.name)).toEqual(['a.scm', 'b.scm', 'c.scm'])
  })
})
