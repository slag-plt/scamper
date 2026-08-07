import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import { describe, expect, test } from 'vitest'

// Regression test for #355: `test/libs/image.browser.test.ts` kept importing
// the canvas pixel functions from src/js/image/image.ts after #347 moved them
// to src/js/canvas/index.ts, and CI was the first thing to notice.
//
// Browser tests sit in a blind spot that ordinary test files do not: they are
// excluded from `npm test` (vite.config.ts skips **/*.browser.test.ts, since
// jsdom cannot back them) and excluded from `npm run typecheck` (the root
// tsconfig.json excludes test/). So nothing local ever loads or typechecks
// them, and a stale import survives all of `npm run validate` only to break
// `npm run test:browser` in CI.
//
// This test runs under the default jsdom suite and checks the one property
// that failure hinged on: every name a browser test imports from a relative
// module is actually exported by that module. It deliberately checks bindings
// rather than typechecking, so it stays fast and is unaffected by unrelated
// pre-existing type errors in the test tree.

const testRoot = resolve(__dirname, '..')

/** Collects every `*.browser.test.ts` file under `test/`. */
function browserTestFiles(): string[] {
  return readdirSync(testRoot, { recursive: true, encoding: 'utf-8' })
    .filter((f) => f.endsWith('.browser.test.ts'))
    .map((f) => resolve(testRoot, f))
    .sort()
}

interface NamedImport {
  /** The name as exported by the target module (before any `as` alias). */
  binding: string
  /** The import specifier as written, e.g. `../../src/js/canvas/index.js`. */
  specifier: string
}

/**
 * Extracts the named bindings a source file imports from relative modules.
 * Namespace (`import * as L`), default, and type-only imports are skipped:
 * only value bindings can break the way #355 broke.
 */
function namedRelativeImports(source: string): NamedImport[] {
  const imports: NamedImport[] = []
  const pattern = /import\s*\{([^}]*)\}\s*from\s*['"]([^'"]+)['"]/g

  for (const [, clause, specifier] of source.matchAll(pattern)) {
    if (!specifier.startsWith('.')) { continue }
    for (const entry of clause.split(',')) {
      const name = entry.trim()
      if (name === '' || name.startsWith('type ')) { continue }
      imports.push({ binding: name.split(/\s+as\s+/)[0].trim(), specifier })
    }
  }

  return imports
}

/** True if `path` names an existing file rather than a directory. */
function isFile(path: string): boolean {
  return existsSync(path) && statSync(path).isFile()
}

/**
 * Resolves an import specifier to a file on disk, mirroring how the bundler
 * reads a `.js` specifier as its `.ts` source and a bare directory as its
 * `index.ts`. The directory check matters for an extensionless specifier like
 * `../../src/lpm`, where the first candidate is the directory itself.
 * @returns the resolved path, or undefined if no candidate exists.
 */
function resolveModule(fromFile: string, specifier: string): string | undefined {
  const base = resolve(dirname(fromFile), specifier)
  const candidates = [
    base.replace(/\.js$/, '.ts'),
    `${base}.ts`,
    `${base}/index.ts`,
  ]
  return candidates.find(isFile)
}

describe('#355: browser tests import names that still exist', () => {
  const files = browserTestFiles()

  test('there is at least one browser test to check', () => {
    expect(files.length).toBeGreaterThan(0)
  })

  test('resolves an extensionless directory specifier to its index.ts', () => {
    const fromFile = resolve(testRoot, 'libs/example.browser.test.ts')
    expect(resolveModule(fromFile, '../../src/lpm')).toBe(
      resolve(testRoot, '../src/lpm/index.ts'),
    )
  })

  test.each(files.map((f) => [f.slice(testRoot.length + 1), f]))(
    '%s imports only names its modules export',
    async (_label, file) => {
      const imports = namedRelativeImports(readFileSync(file, 'utf-8'))

      // Group by specifier so each module is imported once.
      const bySpecifier = new Map<string, string[]>()
      for (const { binding, specifier } of imports) {
        bySpecifier.set(specifier, [
          ...(bySpecifier.get(specifier) ?? []),
          binding,
        ])
      }

      for (const [specifier, bindings] of bySpecifier) {
        const path = resolveModule(file, specifier)
        if (path === undefined) {
          expect.fail(`${specifier} does not resolve to a file`)
        }

        const module = (await import(
          /* @vite-ignore */ pathToFileURL(path).href
        )) as Record<string, unknown>
        const missing = bindings.filter((b) => !(b in module))

        expect(
          missing,
          `${specifier} no longer exports: ${missing.join(', ')}`,
        ).toEqual([])
      }
    },
  )
})
