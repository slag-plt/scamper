/**
 * Raised when a request to the file server never got an answer.
 *
 * Distinct from every other failure for the same reason `NotSignedInError` is:
 * the files are fine, the request was well formed, and nothing is broken. The
 * server is down, or the network is, and the answer is to say so and carry on
 * rather than to show a fault. A blocked-off IDE is the wrong response to a
 * dropped wifi connection.
 *
 * `fetch` rejects with a bare `TypeError` for this -- indistinguishable from a
 * programming mistake at the call site -- so it is converted here, once, by the
 * wrapper both server-backed modules go through.
 */
export class ServerUnreachableError extends Error {
  constructor() {
    super('Scamper cannot reach the server. Check your connection.')
    this.name = 'ServerUnreachableError'
  }
}

/** @returns true iff `error` is the file server being unreachable. */
export function isUnreachable(error: unknown): boolean {
  return error instanceof ServerUnreachableError
}

/**
 * The statuses that mean "nothing usable answered", as opposed to "the server
 * answered and it went badly".
 *
 * - **502** is the proxy in front of the server reporting that the server is
 *   not there. It is what every deploy looks like from the browser:
 *   `scripts/server/web-update` and `server-up --build` leave Caddy serving
 *   while the API container is replaced.
 * - **503** is the server saying its own storage is out of reach (see
 *   `server/src/api.ts`).
 * - **504** is that same proxy giving up waiting.
 *
 * These are exactly the ones `connectivity.ts` treats as offline when it
 * probes, and the two layers have to agree: a heartbeat that says offline while
 * a save reports a hard error would put an undismissable error screen over an
 * editor whose sidebar reads "offline".
 */
const UNREACHABLE_STATUSES = new Set([502, 503, 504])

/**
 * `fetch`, with every way of not getting a usable answer raised as
 * `ServerUnreachableError`: no reply at all, or one of the statuses above.
 *
 * A 500 is deliberately *not* converted. That means the server answered and
 * something is wrong with it, which is a fault worth reporting as one rather
 * than dressing up as a network problem.
 */
export async function fetchServer(
  url: string,
  init?: RequestInit,
): Promise<Response> {
  let response: Response
  try {
    response = await fetch(url, init)
  } catch {
    throw new ServerUnreachableError()
  }

  if (UNREACHABLE_STATUSES.has(response.status)) {
    throw new ServerUnreachableError()
  }
  return response
}
