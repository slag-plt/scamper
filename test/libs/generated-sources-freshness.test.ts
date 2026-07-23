import { readFileSync } from 'fs'
import { resolve } from 'path'
import { describe, expect, test } from 'vitest'
import { buildLibSourcesContent } from '../../scripts/generate-lib-sources.mjs'

describe('generated lib sources freshness', () => {
  test('src/lib/generated/sources.ts matches what src/lib/*.scm currently generates', () => {
    const checkedIn = readFileSync(
      resolve(__dirname, '../../src/lib/generated/sources.ts'),
      'utf-8',
    )

    expect(
      checkedIn,
      'src/lib/generated/sources.ts is stale: run `npm run generate-lib-sources` after editing src/lib/*.scm',
    ).toBe(buildLibSourcesContent())
  })
})
