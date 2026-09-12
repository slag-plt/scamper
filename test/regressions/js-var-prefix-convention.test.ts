import { existsSync, readdirSync } from 'node:fs'
import { resolve } from 'node:path'
import { describe, expect, test } from 'vitest'
import { lookup } from '../../src/js/index.js'

// #548: src/js/index.ts builds the map `js-var` resolves against by flattening
// `Object.entries()` of every library module into it, and states the rule that
// makes that safe -- "Every binding name below is prefixed so that flattening
// all modules into a single map can't collide." Nothing checked it.
// `sampleSourceNode`, a helper that was never meant to be a Scamper binding,
// was exported unprefixed from src/js/audio/index.ts and so sat in the map
// under that bare name, reachable as `(js-var "sampleSourceNode")`.
//
// The convention is load-bearing rather than tidy: the prefix is the only thing
// keeping two libraries' helpers from claiming one key, and anything a
// flattened module exports is a callable binding whether or not anyone meant it
// to be. src/js/browser.ts lives outside every index.ts for exactly that reason
// (#516), as do image/context.ts and image/decode.ts -- reasoning recorded in
// comments and, until this file, enforced nowhere.

const jsRoot = resolve(__dirname, '../../src/js')

/**
 * The library modules src/js/index.ts flattens into the map, read off the
 * directory listing rather than a list written here: a module added later is
 * covered without anyone remembering to come back.
 */
function libraryModules(): string[] {
  return readdirSync(jsRoot, { withFileTypes: true })
    .filter(
      (e) => e.isDirectory() && existsSync(resolve(jsRoot, e.name, 'index.ts')),
    )
    .map((e) => e.name)
    .sort()
}

// A prefix names the *concept*, which for most modules is the module's own
// name. src/js/image/ is the exception: it backs the `image` Scheme library,
// which covers four concepts, so it exports `drawing_*`, `color_*`, `font_*`
// and `image_*` -- see #103 and the note in src/js/index.ts. A module that
// needs a prefix beyond its own name belongs here, with the reason.
const extraPrefixes = new Map<string, string[]>([
  ['image', ['drawing_', 'color_', 'font_']],
])

const prefixesFor = (module: string): string[] => [
  `${module}_`,
  ...(extraPrefixes.get(module) ?? []),
]

/** Whether `name` resolves through the map, i.e. is a Scamper binding. */
function isBinding(name: string): boolean {
  try {
    lookup(name)
    return true
  } catch {
    return false
  }
}

/**
 * Every name a module contributes to the map: its own exports, plus the keys of
 * any record it exports that index.ts flattens in turn -- prelude's comparator
 * groups, which carry the dynamically-named bindings a JS module cannot declare
 * one top-level export at a time.
 *
 * Filtered by what the map actually holds, so an export index.ts deliberately
 * leaves out (those same container records) is not mistaken for a binding.
 */
async function bindingsOf(module: string): Promise<string[]> {
  const exports = (await import(`../../src/js/${module}/index.ts`)) as Record<
    string,
    unknown
  >
  const nested = Object.values(exports).flatMap((v) =>
    typeof v === 'object' && v !== null ? Object.keys(v) : [],
  )
  return [...new Set([...Object.keys(exports), ...nested])].filter(isBinding)
}

describe("#548: every `js-var` binding carries its library's prefix", () => {
  const modules = libraryModules()

  test('the modules and the map are both really there', () => {
    // Guards the whole file against passing vacuously -- an enumeration that
    // silently came back empty, or a `lookup` that answers no to everything.
    expect(modules).toContain('prelude')
    expect(modules.length).toBeGreaterThan(1)
    expect(isBinding('prelude_numberQ')).toBe(true)
  })

  test.each(modules)('%s puts nothing unprefixed in the map', async (module) => {
    const offenders = (await bindingsOf(module)).filter(
      (name) => !prefixesFor(module).some((p) => name.startsWith(p)),
    )
    expect(
      offenders,
      `src/js/${module}/index.ts exports these unprefixed, so they are ` +
        `bindings: ${offenders.join(', ')}. Prefix each with one of ` +
        `${prefixesFor(module).join(', ')}, or move it out of index.ts's ` +
        'reach if it is a helper rather than a Scamper binding.',
    ).toEqual([])
  })

  test('every module is wired into the map', async () => {
    // The check above is vacuous for a module src/js/index.ts never spreads in,
    // which is a bug in its own right: its library's `js-var` lookups all fail.
    for (const module of modules) {
      expect(
        (await bindingsOf(module)).length,
        `src/js/${module}/ has an index.ts but contributes no binding -- is it missing from \`internals\` in src/js/index.ts?`,
      ).toBeGreaterThan(0)
    }
  })

  test('helpers kept outside every index.ts stay out of the map', () => {
    // The other half of the discipline: these are deliberately placed where
    // index.ts cannot reach them, so they are not bindings named after a
    // TypeScript helper. A later `export * from './browser.js'` would make
    // `(js-var "requireBrowser")` resolve, and nothing else would notice.
    for (const name of [
      'requireBrowser',
      'loadImage',
      'imageToCanvas',
      'context2d',
    ]) {
      expect(
        isBinding(name),
        `(js-var "${name}") resolves: a helper has reached the map`,
      ).toBe(false)
    }
  })
})
