import { afterEach, describe, expect, test, vi } from 'vitest'
import { CONFIG_PATH, loadServerConfig } from '../../src/fs/config'

/** Stubs fetch with a reply for the config request. */
function replyWith(reply: Partial<Response> & { json?: () => Promise<unknown> }): void {
  vi.stubGlobal('fetch', () =>
    Promise.resolve({ ok: true, status: 200, ...reply } as Response),
  )
}

afterEach(() => {
  vi.unstubAllGlobals()
})

describe('loadServerConfig', () => {
  test('reads the server URL a deployment advertises', async () => {
    replyWith({ json: () => Promise.resolve({ serverUrl: 'https://files.example/api/v1' }) })

    expect(await loadServerConfig()).toEqual({
      serverUrl: 'https://files.example/api/v1',
    })
  })

  test('is fetched from the site root, not the versioned directory', async () => {
    // scripts/deploy puts each release in its own directory and every past
    // release stays live, so one file at the root re-points all of them at
    // once. A copy per version would mean editing one file per release.
    const fetched: string[] = []
    vi.stubGlobal('fetch', (input: string | URL) => {
      fetched.push(input.toString())
      return Promise.resolve({
        ok: true,
        status: 200,
        json: () => Promise.resolve({ serverUrl: 'https://x/api/v1' }),
      } as Response)
    })

    await loadServerConfig()

    expect(CONFIG_PATH).toBe('/config.json')
    expect(fetched).toEqual(['/config.json'])
  })

  test('no config means no server, not an error', async () => {
    // The common case: a `npm run dev` checkout has no config at all, and a
    // logged-out student must never see this surface as a failure.
    vi.stubGlobal('fetch', () =>
      Promise.resolve({ ok: false, status: 404 } as Response),
    )

    expect(await loadServerConfig()).toBeNull()
  })

  test('a network failure means no server', async () => {
    vi.stubGlobal('fetch', () => Promise.reject(new Error('offline')))

    expect(await loadServerConfig()).toBeNull()
  })

  test('an HTML error page served as 200 does not parse as config', async () => {
    replyWith({ json: () => Promise.reject(new SyntaxError('Unexpected token <')) })

    expect(await loadServerConfig()).toBeNull()
  })

  test('a config without a usable serverUrl is ignored', async () => {
    for (const body of [{}, { serverUrl: '' }, { serverUrl: 42 }, null, 'nope']) {
      replyWith({ json: () => Promise.resolve(body) })
      expect(await loadServerConfig()).toBeNull()
    }
  })
})
