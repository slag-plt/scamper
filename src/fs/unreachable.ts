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
 * `fetch`, with the two ways of not getting an answer raised as
 * `ServerUnreachableError`: no reply at all, and a 503.
 *
 * The 503 belongs here because the server sends it for precisely one thing --
 * its storage is out of reach (see `server/src/api.ts`) -- and from the
 * editor's side that is indistinguishable from the server itself being away.
 * The files cannot be had either way, and the honest thing to say is the same.
 *
 * A 500 is deliberately *not* converted. That means the server answered and
 * something is wrong with it, which is a fault worth reporting as one.
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

  if (response.status === 503) throw new ServerUnreachableError()
  return response
}
