// The IDE's index.html lives under src/app/web/ rather than the project
// root (see flattenHtmlPlugin for why the build output is unaffected), so
// the dev server has nothing to serve at `/` by default. This redirects
// `/` to the IDE during `vite`/`npm run dev` so the plain dev server URL
// still opens the IDE, matching pre-move behavior. `idePath` is passed in
// by vite.config.ts rather than hardcoded here, so this can't drift out of
// sync with the build's own input mapping for the IDE entry.
export function devRootRedirectPlugin(idePath) {
  return {
    name: 'dev-root-redirect',
    configureServer(server) {
      server.middlewares.use((req, res, next) => {
        if (req.url === '/') {
          res.statusCode = 302
          res.setHeader('Location', idePath)
          res.end()
          return
        }
        next()
      })
    },
  }
}
