// Whose request this is.
//
// Its own module rather than a function inside `index.ts`, for the reason
// `api.ts` gives for holding the 401 rule: importing `index.ts` starts a
// listening server, so anything defined there cannot be tested. What this
// decides -- that a database failure is *not* the same as "not signed in" --
// is worth pinning, because getting it wrong is invisible. It looks exactly
// like a lapsed session.

import { fromNodeHeaders } from 'better-auth/node'
import type { IncomingHttpHeaders } from 'node:http'

import type { Auth } from './auth'

/**
 * @returns the id of the user this request is for, or null if it carries no
 *          usable session
 *
 * Null covers two cases the caller must then tell apart: nobody is signed in,
 * and the session could not be read at all. Reading one is a database query,
 * and BetterAuth converts every failure of it into a thrown error -- so an
 * unreachable database arrives here as an exception rather than an absence.
 * Letting that propagate answers 500, which the IDE shows as a fault: an error
 * over the editor, for something that is not the student's doing and will fix
 * itself. Returning null instead lets the route layer ask whether the store is
 * reachable and answer 503, which the IDE reads as being offline.
 *
 * @param onError called when the session could not be read, so a failure that
 *        is *not* the database still surfaces rather than passing as a logout
 */
export async function sessionUserId(
  auth: Auth,
  headers: IncomingHttpHeaders,
  onError: (error: unknown) => void = () => undefined,
): Promise<string | null> {
  try {
    const session = await auth.api.getSession({
      headers: fromNodeHeaders(headers),
    })
    return session?.user.id ?? null
  } catch (error) {
    onError(error)
    return null
  }
}
