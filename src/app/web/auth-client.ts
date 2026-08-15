// Signing in, from the browser's side.
//
// A thin wrapper over BetterAuth's client rather than the client itself, so the
// IDE depends on four functions instead of a library surface, and so the two
// details that are ours -- where the server is, and which methods it offers --
// live in one place.

import { createAuthClient } from 'better-auth/client'

/**
 * Which ways in a server offers, as `/api/v1/auth/methods` reports them.
 *
 * One field, and it stays an object rather than a bare boolean because what it
 * answers -- "can anyone sign in to this server, and how?" -- is the kind of
 * question that grows a second answer later.
 */
export interface SignInMethods {
  password: boolean
}

/** The signed-in user, as much of them as the IDE needs. */
export interface SessionUser {
  id: string
  name: string
  email: string
}

/**
 * BetterAuth mounts its routes at `/api/auth`, a sibling of `/api/v1` rather
 * than a child: sessions are not versioned the way the file API is, because a
 * cookie is a cookie. `serverUrl` is `<origin>/api/v1`, so drop the last two
 * segments to find it.
 */
function authBase(serverUrl: string): string {
  const root = serverUrl.replace(/\/+$/, '').replace(/\/api\/v\d+$/, '')
  // Resolved against this page's origin, because `serverUrl` is normally a
  // path -- the server and the static site share a host -- and BetterAuth's
  // client requires an absolute URL.
  return new URL(`${root}/api/auth`, window.location.origin).toString()
}

/** @returns a client for the server the deployment advertises */
export function createClient(serverUrl: string) {
  return createAuthClient({ baseURL: authBase(serverUrl) })
}

export type AuthClient = ReturnType<typeof createClient>

/**
 * @returns the signed-in user, or null if nobody is
 *
 * Never throws: an unreachable server or a lapsed session both mean "not
 * signed in", and the IDE's answer to both is the same -- carry on with local
 * storage and offer to sign in.
 */
export async function currentUser(
  client: AuthClient,
): Promise<SessionUser | null> {
  try {
    const { data } = await client.getSession()
    if (!data) return null
    const { id, name, email } = data.user
    return { id, name, email }
  } catch {
    return null
  }
}

/**
 * @returns which sign-in methods the server offers, or **null** if it could not
 *          be asked -- unreachable, an error status, or a reply that is not a
 *          list of methods
 *
 * The null matters. "No methods" and "no answer" look alike but mean opposite
 * things: the first is the development stub, which is open and has no accounts,
 * and the second is a server we know nothing about. Collapsing them would put a
 * user onto a file system that answers 401 to everything, with no working way
 * to sign in.
 */
export async function signInMethods(
  serverUrl: string,
): Promise<SignInMethods | null> {
  try {
    const response = await fetch(`${serverUrl.replace(/\/+$/, '')}/auth/methods`)
    if (!response.ok) return null

    const body: unknown = await response.json()
    if (typeof body !== 'object' || body === null) return null

    const methods = body as Partial<SignInMethods>
    return typeof methods.password === 'boolean'
      ? { password: methods.password }
      : null
  } catch {
    return null
  }
}

/**
 * @returns an error message, or null if the sign-in succeeded
 *
 * A server that cannot be reached is reported as a message like any other
 * refusal: the dialog is already open with the form in it, and it is the right
 * place to say why nothing happened.
 */
export async function signInWithPassword(
  client: AuthClient,
  email: string,
  password: string,
): Promise<string | null> {
  try {
    const { error } = await client.signIn.email({ email, password })
    return error ? (error.message ?? 'Could not sign in.') : null
  } catch {
    return 'Scamper cannot reach the server. Check your connection and try again.'
  }
}

/**
 * Ends the session, as far as the server is concerned.
 *
 * A failure is swallowed on purpose: the caller reloads onto local storage
 * either way, and someone signing out on a shared machine has to be able to do
 * it whether or not the network is there. The cookie outliving the request is
 * the cost, and the session it names expires on its own.
 */
export async function signOut(client: AuthClient): Promise<void> {
  try {
    await client.signOut()
  } catch {
    // Deliberately ignored; see above.
  }
}
