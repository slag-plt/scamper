import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { describe, expect, test } from 'vitest'

// Regression test for #637: Vite's `configLoader: 'native'` hands a config
// straight to Node as an ES module, where `__dirname` does not exist and a
// relative import needs its file extension. Both spellings warned on every
// `npm run build` and `npm test`, and stop working outright once the native
// loader becomes Vite's default. `import.meta.dirname` is the replacement.

const repoRoot = resolve(__dirname, '../..')

/** Every file Vite (or vitest) loads as a config. */
const configs = [
  'vite.config.ts',
  'vite.config.embed.ts',
  'test/vitest.browser.config.ts',
]

const sourceOf = (config: string) =>
  readFileSync(resolve(repoRoot, config), 'utf-8')

/** The `from '...'` specifiers naming a file in this repo rather than a package. */
const relativeImports = (source: string) =>
  [...source.matchAll(/from '(\.[^']*)'/g)].map((m) => m[1])

describe('#637: the Vite configs load under the native config loader', () => {
  test.each(configs)('%s uses import.meta.dirname, not __dirname', (config) => {
    expect(sourceOf(config)).not.toMatch(/\b__dirname\b/)
  })

  test.each(configs)('%s imports with file extensions', (config) => {
    for (const spec of relativeImports(sourceOf(config))) {
      expect(spec).toMatch(/\.(ts|mjs|js)$/)
    }
  })

  // So a regex that stopped matching cannot leave the test above vacuous.
  test('there are relative imports to check', () => {
    const found = configs.flatMap((c) => relativeImports(sourceOf(c)))
    expect(found.length).toBeGreaterThan(0)
  })
})
