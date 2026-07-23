import { resolve } from 'path'
import { generateLibSources, libDir } from './generate-lib-sources.mjs'

// Regenerates src/lib/generated/sources.ts from src/lib/*.scm before every
// dev-server/test run, and live-reloads the dev server whenever a library's
// .scm file changes. The *.scm files aren't imported by any module, so
// Vite's module graph never sees them -- we watch the directory explicitly.
//
// Skipped for `vite build`: `npm run build`'s `prebuild` hook (see
// package.json) already regenerates the sources before `vue-tsc`/`vite build`
// run, so regenerating again here would just redo the same work.
export function libSourcesPlugin() {
  let command
  return {
    name: 'lib-sources-generator',
    configResolved(config) {
      command = config.command
    },
    buildStart() {
      if (command === 'build') return
      generateLibSources()
    },
    configureServer(server) {
      server.watcher.on('change', (file) => {
        if (resolve(file, '..') !== libDir || !file.endsWith('.scm')) return
        try {
          generateLibSources()
          server.ws.send({ type: 'full-reload' })
        } catch (err) {
          server.config.logger.error(
            `[lib-sources-generator] failed to regenerate sources from src/lib/*.scm:\n${err.message}`,
            { error: err },
          )
        }
      })
    },
  }
}
