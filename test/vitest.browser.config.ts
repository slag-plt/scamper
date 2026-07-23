// A separate, opt-in test config for the small slice of the standard library
// that needs a real browser's Canvas2D/font-metrics implementation to test
// meaningfully (see test/TESTING.md). Deliberately NOT part of `npm test`/
// `npm run validate` -- a missing Playwright browser binary fails vitest's
// browser-mode startup outright, so folding this into the default run would
// break `npm test` for anyone who hasn't run `npm run playwright:install`.
// Run explicitly via `npm run test:browser` / `npm run coverage:browser`.
//
// N.B., this config lives in test/ (rather than the repo root) so it is
// covered by test/tsconfig.json; a root-level config .ts would push ESLint's
// allowDefaultProject list past its file limit. `root` is pinned back to the
// repo root so include globs and coverage paths stay repo-relative.
import { resolve } from 'path'
import { defineConfig } from 'vitest/config'
import { playwright } from '@vitest/browser-playwright'

export default defineConfig({
  root: resolve(import.meta.dirname, '..'),
  test: {
    include: ['test/**/*.browser.test.ts'],
    browser: {
      enabled: true,
      provider: playwright(),
      headless: true,
      instances: [{ browser: 'chromium' }],
    },
    coverage: {
      provider: 'v8',
      reporter: ['lcov', 'text'],
    },
  },
})
