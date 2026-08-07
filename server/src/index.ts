import { createServer } from 'node:http'

import { API_ROOT, route } from './api'

/** The port to listen on. PORT overrides it wherever this gets deployed. */
const PORT = Number(process.env.PORT ?? 3000)

const server = createServer((req, res) => {
  // For an ordinary request `req.url` is a path plus query rather than an
  // absolute URL, so the parser needs a base it otherwise makes no use of.
  const url = new URL(req.url ?? '/', `http://${req.headers.host ?? 'localhost'}`)
  const { status, body } = route(url.pathname)
  res.writeHead(status, { 'Content-Type': 'application/json' })
  res.end(JSON.stringify(body))
})

server.listen(PORT, () => {
  console.log(
    `Scamper server listening on http://localhost:${PORT.toString()}${API_ROOT}`,
  )
})
