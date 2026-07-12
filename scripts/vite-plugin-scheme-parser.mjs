import { resolve } from "path"
import { generateParser, grammarPath } from "./generate-parser.mjs"

// Regenerates src/scheme/generated/parser.ts from src/scheme/syntax.grammar
// before every build/dev-server/test run, and live-reloads the dev server
// whenever the grammar file changes. syntax.grammar isn't imported by any
// module, so Vite's module graph never sees it -- we watch it explicitly.
export function schemeParserPlugin() {
  return {
    name: "scheme-parser-generator",
    buildStart() {
      generateParser()
    },
    configureServer(server) {
      server.watcher.on("change", (file) => {
        if (resolve(file) !== grammarPath) return
        try {
          generateParser()
          server.ws.send({ type: "full-reload" })
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
