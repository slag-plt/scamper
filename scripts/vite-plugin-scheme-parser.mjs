import { resolve } from 'path'
import { generateParser, grammarPath } from './generate-parser.mjs'

// Regenerates src/scheme/generated/parser.ts from src/scheme/syntax.grammar
// before every dev-server/test run, and live-reloads the dev server whenever
// the grammar file changes. syntax.grammar isn't imported by any module, so
// Vite's module graph never sees it -- we watch it explicitly.
//
// Skipped for `vite build`: `npm run build`'s `prebuild` hook (see
// package.json) already regenerates the parser before `vue-tsc`/`vite build`
// run, so regenerating again here would just redo the same work.
export function schemeParserPlugin() {
  let command
  return {
    name: 'scheme-parser-generator',
    configResolved(config) {
      command = config.command
    },
    buildStart() {
      if (command === 'build') return
      generateParser()
    },
    configureServer(server) {
      server.watcher.on('change', (file) => {
        if (resolve(file) !== grammarPath) return
        try {
          generateParser()
          server.ws.send({ type: 'full-reload' })
        } catch (err) {
          server.config.logger.error(
            `[scheme-parser-generator] failed to regenerate parser from syntax.grammar:\n${err.message}`,
            { error: err },
          )
        }
      })
    },
  }
}
