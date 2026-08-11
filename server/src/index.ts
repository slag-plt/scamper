import { createServer, type IncomingMessage } from 'node:http'
import { fromNodeHeaders, toNodeHandler } from 'better-auth/node'

import { API_ROOT, route } from './api'
import { createAuth, type Auth } from './auth'
import { applySchema, connect } from './db'
import { authBaseUrl, authSecret } from './env'
import { MemoryFileStore } from './store'
import { MemoryHistoryStore } from './history-store'
import { MariaDbFileStore, MariaDbHistoryStore } from './mariadb-stores'
import type { Stores } from './stores'

/** The port to listen on. PORT overrides it wherever this gets deployed. */
const PORT = Number(process.env.PORT ?? 3000)

/**
 * The largest request body accepted, so a stray upload cannot exhaust memory.
 * Scamper files are a few KB; this is generous.
 */
const MAX_BODY_BYTES = 5 * 1024 * 1024

/** Where BetterAuth's own routes live. Its handler owns everything below it. */
const AUTH_ROOT = '/api/auth'

/**
 * The id every request is attributed to when the server runs without a
 * database. There is no sign-in then, so there is one user, and saying so
 * explicitly beats scattering "if unauthenticated" through the stores.
 */
const STUB_USER_ID = 'stub-user'

/**
 * Chooses storage from the environment.
 *
 * With `DATABASE_URL` the server is the real thing: MariaDB, and BetterAuth
 * sessions deciding whose files a request sees. Without it, everything is in
 * memory and unauthenticated -- convenient for working on the front end, and
 * ruinous if it ever happened in production by accident. So it is not a silent
 * fallback: a server with no database refuses to start unless `SCAMPER_STUB=1`
 * says that is what was wanted.
 */
async function configure(): Promise<{ stores: Stores; auth: Auth | null }> {
  const url = process.env.DATABASE_URL

  if (url === undefined || url === '') {
    if (process.env.SCAMPER_STUB !== '1') {
      throw new Error(
        'DATABASE_URL is not set, so there is nowhere to keep anyone\'s files.\n' +
          'Set it, or set SCAMPER_STUB=1 to run in memory with no sign-in ' +
          '(development only -- every request shares one namespace).',
      )
    }
    return {
      stores: {
        files: new MemoryFileStore(),
        history: new MemoryHistoryStore(),
      },
      auth: null,
    }
  }

  await applySchema(url)
  const db = connect(url)
  return {
    stores: {
      files: new MariaDbFileStore(db.sql),
      history: new MariaDbHistoryStore(db.sql),
    },
    auth: createAuth(db.pool, authSecret(), authBaseUrl()),
  }
}

const { stores, auth } = await configure()
const authHandler = auth === null ? null : toNodeHandler(auth)

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

/**
 * @returns the id of the user this request is for, or null if it carries no
 *          valid session. Without auth configured there is only one user.
 */
async function userOf(req: IncomingMessage): Promise<string | null> {
  if (auth === null) return STUB_USER_ID

  const session = await auth.api.getSession({
    headers: fromNodeHeaders(req.headers),
  })
  return session?.user.id ?? null
}

const server = createServer((req, res) => {
  void (async () => {
    // For an ordinary request `req.url` is a path plus query rather than an
    // absolute URL, so the parser needs a base it otherwise makes no use of.
    const url = new URL(
      req.url ?? '/',
      `http://${req.headers.host ?? 'localhost'}`,
    )
    const method = req.method ?? 'GET'

    // Nothing is served cross-origin -- the static site and this server share a
    // host (see README) -- so no reply carries CORS headers and a preflight is
    // never a request this server should be answering.
    if (method === 'OPTIONS') {
      res.writeHead(405)
      res.end()
      return
    }

    // Which ways in this server offers, so the login form does not show a
    // button that cannot work. Answered without a session, like health --
    // it is asked before anyone has signed in. Handled here rather than in
    // api.ts because it reports how *this process* is configured, which the
    // route layer deliberately knows nothing about.
    if (url.pathname === `${API_ROOT}/auth/methods`) {
      if (method !== 'GET') {
        res.writeHead(405, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ error: `Method not allowed: ${method}` }))
        return
      }
      res.writeHead(200, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({ password: auth !== null }))
      return
    }

    // BetterAuth owns everything under /api/auth: sign-up, sign-in, sign-out,
    // session. It reads the request stream itself, so it has to come before
    // readBody() below consumes it.
    if (url.pathname.startsWith(`${AUTH_ROOT}/`) || url.pathname === AUTH_ROOT) {
      if (authHandler === null) {
        res.writeHead(404, { 'Content-Type': 'application/json' })
        res.end(JSON.stringify({ error: 'This server has no sign-in' }))
        return
      }
      await authHandler(req, res)
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

    const { status, body: reply } = await route(
      {
        method,
        path: url.pathname,
        body,
        now: new Date(),
        userId: await userOf(req),
      },
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
    auth === null
      ? 'Storage is the in-memory stub: no sign-in, no persistence, one shared namespace.'
      : 'Storage is MariaDB, and requests need a session.',
  )
  if (auth !== null) {
    // No self-service sign-up and no mail, so say how anyone gets in at all.
    console.log(
      'Sign-in: email + password. Accounts are made with `npm run account -- ' +
        'create <email> <name>`.',
    )
  }
})
