import { describe, expect, test, vi } from 'vitest'
import { sessionUserId } from '../../server/src/session-user'
import type { Auth } from '../../server/src/auth'

// Who a request belongs to, and -- the part worth pinning -- what happens when
// that cannot be determined at all.
//
// BetterAuth reads a session with a database query and converts every failure
// of it into a thrown error, so an unreachable database arrives as an exception
// rather than as an absent session. Letting it propagate answers 500, which the
// IDE shows as an error over the editor; the route layer instead wants a null
// it can turn into 503 by asking whether the store is reachable.

/** An `auth` whose session lookup does whatever the test needs. */
function authWith(getSession: () => Promise<unknown>): Auth {
  return { api: { getSession } } as unknown as Auth
}

describe('sessionUserId', () => {
  test('a signed-in request is its user', async () => {
    const auth = authWith(() => Promise.resolve({ user: { id: 'user-1' } }))

    expect(await sessionUserId(auth, {})).toBe('user-1')
  })

  test('no session is null', async () => {
    const auth = authWith(() => Promise.resolve(null))

    expect(await sessionUserId(auth, {})).toBeNull()
  })

  // The regression this module exists for. Before, the throw propagated and the
  // request answered 500 -- so a database outage put an undismissable error
  // over a student's editor instead of the offline state built for it.
  test('a session that cannot be read is null, not a thrown 500', async () => {
    const auth = authWith(() =>
      Promise.reject(new Error('Failed to get session')),
    )

    await expect(sessionUserId(auth, {})).resolves.toBeNull()
  })

  // Swallowing it silently would hide a genuine bug in the same shape as an
  // outage, so the caller is told either way and logs it.
  test('the failure is reported rather than swallowed', async () => {
    const failure = new Error('Failed to get session')
    const auth = authWith(() => Promise.reject(failure))
    const onError = vi.fn()

    await sessionUserId(auth, {}, onError)

    expect(onError).toHaveBeenCalledWith(failure)
  })

  test('a successful read reports nothing', async () => {
    const auth = authWith(() => Promise.resolve({ user: { id: 'user-1' } }))
    const onError = vi.fn()

    await sessionUserId(auth, {}, onError)

    expect(onError).not.toHaveBeenCalled()
  })
})
