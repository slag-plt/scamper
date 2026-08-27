/**
 * Serves the site-root `/config.json` from the dev server.
 *
 * In production this file is written once, outside any version's directory, by
 * `scripts/set-server-url`; a dev checkout has no such file, which is why
 * `npm run dev` stays on local storage. This plugin supplies one so
 * `npm run dev:memory` can exercise the server backend, and is only installed in
 * that mode.
 *
 * The URL it advertises is relative on purpose. The dev server proxies `/api`
 * to the back end (see vite.config.ts), so the browser only ever talks to one
 * origin -- the same arrangement production has, where the static site and the
 * API are the same host. Cookies, CORS, and `SameSite` therefore behave in dev
 * exactly as they will in production, which is the point of proxying rather
 * than pointing the client straight at :3000.
 *
 * @param {string} serverUrl the API root to advertise, e.g. `/api/v1`
 * @returns {import('vite').Plugin}
 */
/**
 * @param {string | undefined} serverUrl
 * @returns {import('vite').Plugin}
 */
export function devServerConfigPlugin(serverUrl) {
  return {
    name: 'scamper-dev-server-config',
    apply: 'serve',
    configureServer(server) {
      server.middlewares.use((req, res, next) => {
        // The query/hash never appears on this request, but be exact anyway.
        if ((req.url ?? '').split('?')[0] !== '/config.json') {
          next()
          return
        }
        res.setHeader('Content-Type', 'application/json')
        res.setHeader('Cache-Control', 'no-store')
        res.end(JSON.stringify({ serverUrl }))
      })
    },
  }
}
