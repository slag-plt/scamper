// A separate, opt-in test config for the small slice of the standard library
// that needs a real browser's Canvas2D/font-metrics implementation to test
// meaningfully (see docs/testing.md). Deliberately NOT part of `npm test`/
// `npm run validate` -- a missing Playwright browser binary fails vitest's
// browser-mode startup outright, so folding this into the default run would
// break `npm test` for anyone who hasn't run `npm run playwright:install`.
// Run explicitly via `npm run test:browser` / `npm run coverage:browser`.
//
// N.B., this config lives in test/ (rather than the repo root) so it is
// covered by test/tsconfig.json; a root-level config .ts would push ESLint's
// allowDefaultProject list past its file limit. `root` is pinned back to the
// repo root so include globs and coverage paths stay repo-relative.
import { readFileSync } from 'fs'
import { resolve } from 'path'
import { defineConfig } from 'vitest/config'
import vue from '@vitejs/plugin-vue'
import { playwright } from '@vitest/browser-playwright'

export default defineConfig({
  root: resolve(import.meta.dirname, '..'),
  // The dock's browser tests mount real components, so .vue files have to be
  // compiled here as they are for the app. Without this Vite hands the SFC to
  // its JS parser and reports it as a syntax error at the closing </script>.
  plugins: [vue()],
  // Mirrors vite.config.ts. Anything mounting IdeApp reads APP_VERSION, and
  // without the define it is a bare ReferenceError at setup time.
  define: {
    APP_VERSION: JSON.stringify(
      (
        JSON.parse(
          readFileSync(resolve(import.meta.dirname, '../package.json'), 'utf-8'),
        ) as { version?: string }
      ).version ?? 'unknown',
    ),
  },
  test: {
    include: ['test/**/*.browser.test.ts'],
    // One browser, and so one origin: every file here shares the same OPFS.
    // Run in parallel, a file that clears storage empties another's fixtures
    // halfway through it -- test/fs/opfs.browser.test.ts does exactly that,
    // and the #429 regression writes real files beside it. The suite is small
    // enough that serialising it costs seconds.
    fileParallelism: false,
    browser: {
      enabled: true,
      provider: playwright(),
      headless: true,
      // A known viewport: some specs assert where something lands after being
      // clamped into the window, which is meaningless at an arbitrary size.
      viewport: { width: 1280, height: 800 },
      instances: [{ browser: 'chromium' }],
    },
    coverage: {
      provider: 'v8',
      reporter: ['lcov', 'text'],
    },
  },
})
