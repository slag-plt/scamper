#!/usr/bin/env node
import { buildParserFile } from '@lezer/generator'
import { mkdirSync, readFileSync, writeFileSync } from 'fs'
import { dirname, join } from 'path'
import { fileURLToPath } from 'url'

const rootDir = dirname(dirname(fileURLToPath(import.meta.url)))
export const grammarPath = join(rootDir, 'src/scheme/syntax.grammar')
const outPath = join(rootDir, 'src/scheme/generated/parser.ts')
const termsPath = join(rootDir, 'src/scheme/generated/parser.terms.ts')

export function generateParser() {
  const grammarSrc = readFileSync(grammarPath, 'utf-8')
  const { parser, terms } = buildParserFile(grammarSrc, {
    fileName: 'syntax.grammar',
    moduleStyle: 'es',
    typeScript: true,
  })

  const header = `// GENERATED FILE -- DO NOT EDIT.
// Regenerated automatically from src/scheme/syntax.grammar by the Vite
// scheme-parser plugin, or manually via \`npm run generate-parser\`.

`

  mkdirSync(dirname(outPath), { recursive: true })
  writeFileSync(outPath, header + parser)
  // The terms module exports the token/node ids used by external tokenizers
  // (src/scheme/anon-tokens.ts imports AnonHash from it).
  writeFileSync(termsPath, header + terms)
  return outPath
}

const isMain = process.argv[1] && import.meta.url === `file://${process.argv[1]}`
if (isMain) {
  const parserPath = generateParser()
  console.log(`Wrote ${parserPath} (+ parser.terms.ts)`)
}
