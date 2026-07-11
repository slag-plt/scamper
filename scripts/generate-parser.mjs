#!/usr/bin/env node
import { buildParserFile } from "@lezer/generator"
import { mkdirSync, readFileSync, writeFileSync } from "fs"
import { dirname, join } from "path"
import { fileURLToPath } from "url"

const rootDir = dirname(dirname(fileURLToPath(import.meta.url)))
const grammarPath = join(rootDir, "src/scheme/syntax.grammar")
const outPath = join(rootDir, "src/scheme/generated/parser.ts")

const grammarSrc = readFileSync(grammarPath, "utf-8")
const { parser } = buildParserFile(grammarSrc, {
  fileName: "syntax.grammar",
  moduleStyle: "es",
  typeScript: true,
})

const header = `// GENERATED FILE -- DO NOT EDIT.
// Regenerate with \`npm run generate-parser\` after editing src/scheme/syntax.grammar.

`

mkdirSync(dirname(outPath), { recursive: true })
writeFileSync(outPath, header + parser)
console.log(`Wrote ${outPath}`)
