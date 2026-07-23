// The IDE's index.html lives at src/app/web/index.html rather than the
// project root (see flattenHtmlPlugin for why the build output is
// unaffected), so the dev server has nothing to serve at `/` by default.
// This redirects `/` to the IDE during `vite`/`npm run dev` so the plain
// dev server URL still opens the IDE, matching pre-move behavior.
export function devRootRedirectPlugin() {
  return {
    name: 'dev-root-redirect',
    configureServer(server) {
      server.middlewares.use((req, res, next) => {
        if (req.url === '/') {
          res.statusCode = 302
          res.setHeader('Location', '/src/app/web/index.html')
          res.end()
          return
        }
        next()
      })
    },
  }
}
