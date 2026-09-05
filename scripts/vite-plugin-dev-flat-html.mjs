import { basename } from 'path'

// In dev, the HTML entry points are served from their source locations
// (src/app/web/, src/app/docs/, src/app/search/, src/app/files/), but the
// production build flattens them to the site root -- index.html, docs.html,
// search.html, files.html, etc. all sit at the top level (see
// flattenHtmlPlugin). This middleware makes the dev server mirror that flat
// layout: each flattened URL (e.g. /docs.html) is served in place from its
// nested source file, and `/` serves the IDE. That way the apps' relative
// cross-page links (`docs.html`, `search.html`, ...) resolve identically in
// dev and in the deployed build, at any base path.
//
// The rewrite is internal (req.url is changed, not redirected), so the browser
// stays on the flat URL. `entryPaths`/`idePath` come from vite.config.ts's
// single entry map, so this can't drift from the build's own input mapping.
/**
 * @param {string[]} entryPaths every HTML entry, at its source location
 * @param {string} idePath the entry `/` should serve
 * @returns {import('vite').Plugin}
 */
export function devFlatHtmlPlugin(entryPaths, idePath) {
  /** @type {Map<string, string>} flat URL -> the source path serving it */
  const rewrites = new Map()
  for (const path of entryPaths) {
    rewrites.set(`/${basename(path)}`, `/${path}`)
  }
  rewrites.set('/', `/${idePath}`)

  return {
    name: 'dev-flat-html',
    configureServer(server) {
      server.middlewares.use((req, res, next) => {
        if (req.url !== undefined) {
          const queryAt = req.url.indexOf('?')
          const path = queryAt === -1 ? req.url : req.url.slice(0, queryAt)
          const target = rewrites.get(path)
          if (target !== undefined) {
            req.url = queryAt === -1 ? target : target + req.url.slice(queryAt)
          }
        }
        next()
      })
    },
  }
}
