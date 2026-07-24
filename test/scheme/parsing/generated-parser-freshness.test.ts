import { buildParserFile } from '@lezer/generator'
import { readFileSync } from 'fs'
import { resolve } from 'path'
import { describe, expect, test } from 'vitest'

describe('generated Lezer parser freshness', () => {
  test('src/scheme/generated/parser.ts matches what syntax.grammar currently generates', () => {
    const grammarSrc = readFileSync(
      resolve(__dirname, '../../../src/scheme/syntax.grammar'),
      'utf-8',
    )
    const checkedIn = readFileSync(
      resolve(__dirname, '../../../src/scheme/generated/parser.ts'),
      'utf-8',
    )

    const { parser } = buildParserFile(grammarSrc, {
      fileName: 'syntax.grammar',
      moduleStyle: 'es',
      typeScript: true,
    })
    const headerEnd = checkedIn.indexOf('// This file was generated')
    const checkedInBody = checkedIn.slice(headerEnd)

    expect(
      checkedInBody,
      'src/scheme/generated/parser.ts is stale: run `npm run generate-parser` after editing src/scheme/syntax.grammar',
    ).toBe(parser)
  })
})
