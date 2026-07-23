#!/usr/bin/env node
import { mkdirSync, readdirSync, readFileSync, writeFileSync } from 'fs'
import { basename, dirname, join } from 'path'
import { fileURLToPath } from 'url'

const rootDir = dirname(dirname(fileURLToPath(import.meta.url)))
export const libDir = join(rootDir, 'src/lib')
const outPath = join(rootDir, 'src/lib/generated/sources.ts')

export function generateLibSources() {
  const files = readdirSync(libDir)
    .filter((f) => f.endsWith('.scm'))
    .sort()

  const entries = files.map((file) => {
    const name = basename(file, '.scm')
    const src = readFileSync(join(libDir, file), 'utf-8')
    return `  [${JSON.stringify(name)}, ${JSON.stringify(src)}],`
  })

  const header = `// GENERATED FILE -- DO NOT EDIT.
// Regenerated automatically from the *.scm files in src/lib by the Vite
// lib-sources plugin, or manually via \`npm run generate-lib-sources\`.

`

  const content =
    header +
    `export const librarySources: [string, string][] = [\n${entries.join('\n')}\n]\n`

  mkdirSync(dirname(outPath), { recursive: true })
  writeFileSync(outPath, content)
  return outPath
}

const isMain = process.argv[1] && import.meta.url === `file://${process.argv[1]}`
if (isMain) {
  console.log(`Wrote ${generateLibSources()}`)
}
