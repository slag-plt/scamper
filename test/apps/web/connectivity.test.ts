import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import * as Connectivity from '../../../src/app/web/connectivity'

// The heartbeat that decides whether the IDE is online (#357). What is worth
// pinning is the state machine, not the timing: which answers count as
// reachable, that a failed request is believed immediately, and that recovery
// happens on its own -- because the alternative to all three is the blocking
// error screen this replaced.

const SERVER_URL = 'https://files.example/api/v1'

/** Answers every probe with `ok`, recording the URLs asked for. */
function respondWith(ok: boolean): { calls: string[] } {
  const calls: string[] = []
  vi.stubGlobal('fetch', (url: string) => {
    calls.push(url)
    return Promise.resolve({ ok, status: ok ? 200 : 502 } as Response)
  })
  return { calls }
}

/** Rejects every probe, as `fetch` does when nothing answers. */
function respondUnreachable(): void {
  vi.stubGlobal('fetch', () => Promise.reject(new TypeError('Failed to fetch')))
}

beforeEach(() => {
  vi.useFakeTimers()
})

afterEach(() => {
  Connectivity.stop()
  vi.useRealTimers()
  vi.unstubAllGlobals()
})

describe('probing', () => {
  test('a healthy server is online', async () => {
    const { calls } = respondWith(true)
    Connectivity.start(SERVER_URL)

    expect(await Connectivity.checkNow()).toBe('online')
    expect(calls[0]).toBe(`${SERVER_URL}/health`)
  })

  test('an unreachable server is offline', async () => {
    respondUnreachable()
    Connectivity.start(SERVER_URL)

    expect(await Connectivity.checkNow()).toBe('offline')
    expect(Connectivity.connection.value).toBe('offline')
  })

  // A proxy that is up while the server behind it is down answers, but not with
  // anything usable -- so a reply is not on its own evidence of being online.
  test('a 502 from the proxy in front of the server is offline', async () => {
    respondWith(false)
    Connectivity.start(SERVER_URL)

    expect(await Connectivity.checkNow()).toBe('offline')
  })

  test('a deployment with no server is always online', async () => {
    respondUnreachable()

    // No start(): this is `npm run dev`, where there is nothing to be out of
    // reach of and nothing should ever be blocked.
    expect(await Connectivity.checkNow()).toBe('online')
    expect(Connectivity.connection.value).toBe('online')
  })
})

describe('reacting to real requests', () => {
  test('a failed request is believed without waiting for a heartbeat', () => {
    respondWith(true)
    Connectivity.start(SERVER_URL)

    Connectivity.reportUnreachable()

    expect(Connectivity.connection.value).toBe('offline')
  })

  test('listeners hear each change once, not each beat', async () => {
    respondUnreachable()
    Connectivity.start(SERVER_URL)
    const seen: string[] = []
    const unsubscribe = Connectivity.onConnectionChange((state) => {
      seen.push(state)
    })

    await Connectivity.checkNow()
    await Connectivity.checkNow()
    expect(seen).toEqual(['offline'])

    respondWith(true)
    await Connectivity.checkNow()
    await Connectivity.checkNow()
    expect(seen).toEqual(['offline', 'online'])

    unsubscribe()
  })
})

describe('recovering', () => {
  test('the heartbeat comes back online by itself', async () => {
    respondUnreachable()
    Connectivity.start(SERVER_URL)
    await vi.advanceTimersByTimeAsync(0)
    expect(Connectivity.connection.value).toBe('offline')

    // The server comes back. Nobody clicks anything; the next beat notices.
    respondWith(true)
    await vi.advanceTimersByTimeAsync(6_000)

    expect(Connectivity.connection.value).toBe('online')
  })

  test('stopping leaves nothing running', async () => {
    const { calls } = respondWith(true)
    Connectivity.start(SERVER_URL)
    await vi.advanceTimersByTimeAsync(0)
    const asked = calls.length

    Connectivity.stop()
    await vi.advanceTimersByTimeAsync(120_000)

    expect(calls.length).toBe(asked)
  })
})
