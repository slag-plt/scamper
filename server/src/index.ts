import { createServer, type IncomingMessage, type ServerResponse } from 'node:http'

import { API_ROOT, route } from './api'
import { FileStore } from './store'
import { HistoryStore } from './history-store'

/** The port to listen on. PORT overrides it wherever this gets deployed. */
const PORT = Number(process.env.PORT ?? 3000)

/**
 * The single origin allowed to call this server with credentials, if any.
 *
 * Unset means no CORS headers at all, which is right when the server and the
 * static site share an origin. Cross-origin hosting has to opt in explicitly:
 * `Access-Control-Allow-Origin` cannot be `*` once the client sends cookies,
 * so this is a single origin rather than a list of them.
 */
const ALLOWED_ORIGIN = process.env.ALLOWED_ORIGIN

/**
 * The largest request body accepted, so a stray upload cannot exhaust memory.
 * Scamper files are a few KB; this is generous.
 */
const MAX_BODY_BYTES = 5 * 1024 * 1024

const stores = {
  files: new FileStore(),
  history: new HistoryStore(),
}

/**
 * Reads and parses a JSON request body.
 * @returns the parsed body, or undefined if there was none or it was malformed
 *          -- either way the route reports the field it wanted as missing
 */
async function readBody(req: IncomingMessage): Promise<unknown> {
  const chunks: Buffer[] = []
  let size = 0

  for await (const chunk of req as AsyncIterable<Buffer>) {
    size += chunk.length
    if (size > MAX_BODY_BYTES) throw new Error('Request body too large')
    chunks.push(chunk)
  }

  if (chunks.length === 0) return undefined

  try {
    return JSON.parse(Buffer.concat(chunks).toString('utf-8'))
  } catch {
    return undefined
  }
}

/** Applies the configured cross-origin headers, if any, to a reply. */
function applyCors(res: ServerResponse): void {
  if (ALLOWED_ORIGIN === undefined) return

  res.setHeader('Access-Control-Allow-Origin', ALLOWED_ORIGIN)
  res.setHeader('Access-Control-Allow-Credentials', 'true')
  res.setHeader('Access-Control-Allow-Methods', 'GET, PUT, POST, DELETE, OPTIONS')
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type')
  // Replies vary by origin, so a cache must not serve one origin's to another.
  res.setHeader('Vary', 'Origin')
}

const server = createServer((req, res) => {
  void (async () => {
    applyCors(res)

    // For an ordinary request `req.url` is a path plus query rather than an
    // absolute URL, so the parser needs a base it otherwise makes no use of.
    const url = new URL(
      req.url ?? '/',
      `http://${req.headers.host ?? 'localhost'}`,
    )
    const method = req.method ?? 'GET'

    // A cross-origin PUT/DELETE carrying JSON is preflighted; answer before
    // any routing, since the browser sends no credentials with a preflight.
    if (method === 'OPTIONS') {
      res.writeHead(ALLOWED_ORIGIN === undefined ? 405 : 204)
      res.end()
      return
    }

    let body: unknown
    try {
      body = await readBody(req)
    } catch {
      res.writeHead(413, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({ error: 'Request body too large' }))
      return
    }

    const { status, body: reply } = route(
      { method, path: url.pathname, body, now: new Date() },
      stores,
    )

    if (reply === undefined) {
      res.writeHead(status)
      res.end()
      return
    }

    res.writeHead(status, { 'Content-Type': 'application/json' })
    res.end(JSON.stringify(reply))
  })()
})

server.listen(PORT, () => {
  console.log(
    `Scamper server listening on http://localhost:${PORT.toString()}${API_ROOT}`,
  )
  console.log(
    'Storage is the in-memory stub: no authentication, no persistence.',
  )
})
