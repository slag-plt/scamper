import type { FileStore } from './store'

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

/** Where the file-system routes live, mirroring the `FS` interface. */
const FS_ROOT = `${API_ROOT}/fs`

/** A reply, with the status to send it under. A body of undefined sends none. */
export interface ApiResponse {
  status: number
  body?: unknown
}

/** A parsed request, independent of how it arrived over HTTP. */
export interface ApiRequest {
  method: string
  /** The request's pathname, still percent-encoded. */
  path: string
  /** The parsed JSON body, or undefined if the request carried none. */
  body?: unknown
}

/** @returns the string at `key` in `body`, or undefined if it is not one */
function field(body: unknown, key: string): string | undefined {
  if (typeof body !== 'object' || body === null) return undefined

  const value = (body as Record<string, unknown>)[key]
  return typeof value === 'string' ? value : undefined
}

/**
 * Dispatches one request against `store`.
 *
 * Kept free of `node:http` so it can be exercised directly: every route is a
 * plain function from a request to a reply.
 *
 * @returns the reply to send, including the 404 for an unclaimed path and the
 *          405 for a path that exists under a method it does not serve
 */
export function route(request: ApiRequest, store: FileStore): ApiResponse {
  const { method, path, body } = request

  if (path === `${API_ROOT}/health`) {
    return { status: 200, body: { status: 'ok', api: API_ROOT } }
  }

  if (path === `${FS_ROOT}/files`) {
    if (method !== 'GET') return methodNotAllowed(method)
    return { status: 200, body: { files: store.list() } }
  }

  if (path === `${FS_ROOT}/rename`) {
    if (method !== 'POST') return methodNotAllowed(method)

    const from = field(body, 'from')
    const to = field(body, 'to')
    if (from === undefined || to === undefined) {
      return badRequest('rename needs string `from` and `to` fields')
    }

    return store.rename(from, to) ? { status: 204 } : notFound(from)
  }

  if (path.startsWith(`${FS_ROOT}/files/`)) {
    // A filename is one percent-encoded path segment, so it round-trips names
    // holding slashes, spaces, or anything else a student types.
    const name = decodeURIComponent(path.slice(`${FS_ROOT}/files/`.length))
    return routeFile(method, name, body, store)
  }

  return { status: 404, body: { error: `No such endpoint: ${path}` } }
}

/** Dispatches the read/write/delete routes for a single named file. */
function routeFile(
  method: string,
  name: string,
  body: unknown,
  store: FileStore,
): ApiResponse {
  switch (method) {
    case 'GET': {
      const contents = store.read(name)
      return contents === undefined
        ? notFound(name)
        : { status: 200, body: { contents } }
    }

    case 'PUT': {
      const contents = field(body, 'contents')
      if (contents === undefined) {
        return badRequest('save needs a string `contents` field')
      }

      store.write(name, contents)
      return { status: 204 }
    }

    case 'DELETE':
      return store.remove(name) ? { status: 204 } : notFound(name)

    default:
      return methodNotAllowed(method)
  }
}

/** @returns the 404 reply for a file the store does not hold */
function notFound(name: string): ApiResponse {
  return { status: 404, body: { error: `No such file: ${name}` } }
}

/** @returns the 400 reply for a request whose body is missing a field */
function badRequest(message: string): ApiResponse {
  return { status: 400, body: { error: message } }
}

/** @returns the 405 reply for a known path under an unsupported method */
function methodNotAllowed(method: string): ApiResponse {
  return { status: 405, body: { error: `Method not allowed: ${method}` } }
}
