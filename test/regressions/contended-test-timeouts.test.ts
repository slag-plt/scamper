import { readdirSync, readFileSync, statSync } from 'node:fs'
import { resolve } from 'node:path'
import { describe, expect, test } from 'vitest'

// Regression test for #536: tests that wait on elapsed time rather than on a
// condition fail on a loaded machine. Reproduced by pinning vitest to one CPU
// along 30 busy workers, which slowed the suite ~11x: four tests in
// test/apps/web/ide-notebook.test.ts, all three in
// test/regressions/gradescope-harness.test.ts, nine of ten in
// test/apps/cli/cli.test.ts and the one in
// test/regressions/trace-step-limit.test.ts went red on a tree with no changes
// in it. Every one passes uncontended.
//
// Two habits cause it, and both are invisible while reading the test that has
// them, which is why they are pinned here rather than left to review:
//
//  1. A test spawns a child process and gives *the child* a generous budget --
//     15s for the CLI, 60s for the Gradescope harness -- while the test around
//     it still runs under vitest's 5s default. The child's budget is therefore
//     unreachable: what actually fails is the test, at 5s, and the number the
//     author wrote never applies. `npx tsx` cold-starting the whole front end
//     takes about a second idle, so 5s is not a hang detector, it is a
//     performance assertion about the machine.
//
//  2. A jsdom test waits a fixed number of ticks for work whose length is not
//     fixed. The notebook publishes a run's output one animation frame at a
//     time (see use-notebook.ts), so `flushPromises()` + one frame is enough
//     only while the program finishes inside that frame. It is worth noting
//     that the suites already using a retrying wait -- results-pane-bugs,
//     ide-archive, ide-hmr-overwrite -- survived the same contention intact.
//
// A timeout is a hang detector. Nothing here asserts that a test is *fast*.

const testRoot = resolve(import.meta.dirname, '..')
const repoRoot = resolve(testRoot, '..')

/**
 * Vitest's own defaults, which are what a suite asking for nothing gets. Both
 * were observed failing under contention: the tests at 5s, and then
 * trace-step-limit's `beforeAll(initialize)` at 10s once the tests had room.
 */
const VITEST_DEFAULTS_MS = { testTimeout: 5_000, hookTimeout: 10_000 }

/**
 * The suites whose waits have to be conditions rather than ticks, because a
 * run's output reaches them a frame at a time and nothing bounds how many
 * frames it takes. Add a file here when contention catches it, as #536 caught
 * the notebook.
 */
const SETTLE_SENSITIVE = ['apps/web/ide-notebook.test.ts']

/** The functions that hand a budget to a child process rather than to vitest. */
const SPAWNERS = ['spawnSync', 'spawn', 'execFileSync', 'execFile', 'execSync']

/** Collects every `*.test.ts` file under `test/`. */
function testFiles(): string[] {
  return readdirSync(testRoot, { recursive: true, encoding: 'utf-8' })
    .filter((f) => f.endsWith('.test.ts'))
    .map((f) => resolve(testRoot, f))
    // A failed browser test leaves a screenshot directory *named* after it,
    // which reads as EISDIR (see browser-test-imports.test.ts).
    .filter((f) => statSync(f).isFile())
    .sort()
}

/** A number written as a TypeScript numeric literal, underscores and all. */
function ms(literal: string): number {
  return Number(literal.replaceAll('_', ''))
}

/** A vitest budget, with where it was written so it can be told apart from a
 *  child process's. */
interface Budget {
  limitMs: number
  /** Index in the source of the number itself. */
  at: number
}

/**
 * The vitest budgets a file declares, in the three spellings this repo uses: a
 * trailing per-test timeout (`}, 20_000)`), an options object on a test or
 * describe (`describe('x', { timeout: 20_000 }, ...)`), and `testTimeout`.
 */
function declaredBudgets(source: string): Budget[] {
  const patterns = [
    /\}\s*,\s*(\d[\d_]*)\s*\)/g,
    /(?:describe|test|it)(?:\.\w+)*\(\s*(?:'[^']*'|"[^"]*"|`[^`]*`)\s*,\s*\{[^{}]*\btimeout:\s*(\d[\d_]*)/g,
    /\btestTimeout:\s*(\d[\d_]*)/g,
  ]
  return patterns.flatMap((pattern) =>
    [...source.matchAll(pattern)].map((m) => ({
      limitMs: ms(m[1]),
      at: m.index + m[0].lastIndexOf(m[1]),
    })),
  )
}

/**
 * The `timeout:` values a file hands to something other than vitest -- in
 * practice a child process, since these are only read out of files that spawn
 * one. Told apart from a vitest budget by position, so the two spellings of
 * `timeout:` cannot be confused for each other.
 */
function nonVitestTimeouts(source: string): number[] {
  const budgets = new Set(declaredBudgets(source).map((b) => b.at))
  return [...source.matchAll(/\btimeout:\s*(\d[\d_]*)/g)]
    .filter((m) => !budgets.has(m.index + m[0].lastIndexOf(m[1])))
    .map((m) => ms(m[1]))
}

/** A suite-wide budget from vite.config.ts, or vitest's default when unset. */
function globalBudgetMs(key: keyof typeof VITEST_DEFAULTS_MS): number {
  const config = readFileSync(resolve(repoRoot, 'vite.config.ts'), 'utf-8')
  const match = new RegExp(`\\b${key}:\\s*(\\d[\\d_]*)`).exec(config)
  return match === null ? VITEST_DEFAULTS_MS[key] : ms(match[1])
}

const spawningFiles = testFiles().filter((file) => {
  const source = readFileSync(file, 'utf-8')
  return SPAWNERS.some((name) => new RegExp(`\\b${name}\\s*\\(`).test(source))
})

describe('#536: a test budget survives a loaded machine', () => {
  // Named in vite.config.ts so it is one decision in one place, and so a
  // reader of a test with no timeout of its own can find out what it gets.
  test.each(['testTimeout', 'hookTimeout'] as const)(
    'the suite sets its own %s instead of inheriting vitest’s',
    (key) => {
      expect(
        globalBudgetMs(key),
        `vite.config.ts sets no ${key}, so every suite runs under vitest’s ` +
          `${VITEST_DEFAULTS_MS[key].toString()}ms default -- a budget a ` +
          'loaded runner blows through on work that takes a second idle',
      ).toBeGreaterThan(VITEST_DEFAULTS_MS[key])
    },
  )

  test('there is at least one subprocess suite to check', () => {
    expect(spawningFiles.length).toBeGreaterThan(0)
  })

  test.each(spawningFiles.map((f) => [f.slice(testRoot.length + 1), f]))(
    '%s outlives the child process it waits for',
    (_label, file) => {
      const source = readFileSync(file, 'utf-8')
      const childMs = nonVitestTimeouts(source)
      const budgetMs = Math.max(
        globalBudgetMs('testTimeout'),
        ...declaredBudgets(source).map((b) => b.limitMs),
      )
      for (const limit of childMs) {
        expect(
          budgetMs,
          `a child process is given ${limit.toString()}ms but the test around ` +
            `it only has ${budgetMs.toString()}ms, so the child's budget can ` +
            'never apply: the test dies first, and does so on any machine slow ' +
            'enough to take the longer path',
        ).toBeGreaterThanOrEqual(limit)
      }
    },
  )

  test.each(SETTLE_SENSITIVE)('%s waits on a condition, not on ticks', (name) => {
    const source = readFileSync(resolve(testRoot, name), 'utf-8')
    // `vi.waitFor` or @testing-library/dom's `waitFor`: either retries until
    // the assertion holds. `flushPromises()` and a hand-rolled frame wait do
    // not -- they allow a fixed number of turns for an unbounded amount of
    // work, which is exactly what contention breaks.
    expect(
      /\bwaitFor\(/.test(source),
      `${name} settles with a fixed number of ticks. Wait on the rendered ` +
        'content instead, so a slow machine costs time rather than a failure',
    ).toBe(true)
  })
})
