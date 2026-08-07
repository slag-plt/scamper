/**
 * The prefix every route lives under.
 *
 * Scamper's front end deploys to a versioned directory per release (see
 * `scripts/deploy`) and old releases stay reachable at their URLs indefinitely,
 * so one server always serves many client versions at once. Namespacing from
 * the first commit means a breaking change can ship as `/api/v2` alongside
 * `/api/v1` instead of stranding clients that will never be rebuilt.
 */
export const API_ROOT = '/api/v1'

/** A JSON reply, paired with the status code to send it under. */
export interface ApiResponse {
  status: number
  body: unknown
}

/**
 * Dispatches a request to its handler.
 * @param path the request's pathname, e.g. `/api/v1/health`
 * @returns the reply to send, including the 404 for an unclaimed path
 */
export function route(path: string): ApiResponse {
  switch (path) {
    case `${API_ROOT}/health`:
      return { status: 200, body: { status: 'ok', api: API_ROOT } }
    default:
      return { status: 404, body: { error: `No such endpoint: ${path}` } }
  }
}
