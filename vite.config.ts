// vite.config.ts
import { resolve } from 'path'
import { defineConfig } from 'vitest/config'

import vue from '@vitejs/plugin-vue'
import { schemeParserPlugin } from './scripts/vite-plugin-scheme-parser.mjs'
import { libSourcesPlugin } from './scripts/vite-plugin-lib-sources.mjs'
import { flattenHtmlPlugin } from './scripts/vite-plugin-flatten-html.mjs'
import { devFlatHtmlPlugin } from './scripts/vite-plugin-dev-flat-html.mjs'

const AppVersion = process.env.npm_package_version ?? 'unknown'

// The single source of truth for the HTML entry points, so the build input,
// the production flattening (flattenHtmlPlugin), and the dev server's flat
// layout (devFlatHtmlPlugin) all stay in sync. Each app's entry lives next to
// its source; the build flattens them to distinct top-level basenames.
const ideEntry = 'src/app/web/index.html'
const htmlEntries: Record<string, string> = {
  'scamper-docs': 'src/app/docs/docs.html',
  'scamper-ide': ideEntry,
  'scamper-runner': 'src/app/web/runner.html',
  'scamper-web': 'src/app/web/web.html',
  'scamper-search': 'src/app/search/search.html',
}

export default defineConfig({
  build: {
    rolldownOptions: {
      input: Object.fromEntries(
        Object.entries(htmlEntries).map(([name, path]) => [
          name,
          resolve(__dirname, path),
        ]),
      ),
      output: {
        entryFileNames: `assets/[name]-${AppVersion}.js`,
        chunkFileNames: `assets/[name]-${AppVersion}.js`,
        assetFileNames: `assets/[name]-${AppVersion}.[ext]`,
      },
    },
  },

  plugins: [
    schemeParserPlugin(),
    libSourcesPlugin(),
    devFlatHtmlPlugin(Object.values(htmlEntries), ideEntry),
    vue(),
    flattenHtmlPlugin(),
  ],

  define: {
    APP_VERSION: JSON.stringify(AppVersion),
  },

  test: {
    environment: 'jsdom',
    setupFiles: './test/setup.ts',
    // *.browser.test.ts files need a real browser's Canvas2D/font-metrics
    // implementation and run separately via `npm run test:browser` (see
    // test/vitest.browser.config.ts) -- excluded here since they'd fail under
    // jsdom's stubbed-out canvas support. .claude holds agent git worktrees
    // (separate checkouts); never collect tests from them.
    exclude: [
      '**/node_modules/**',
      '**/.git/**',
      '**/.claude/**',
      '**/*.browser.test.ts',
    ],
    coverage: {
      provider: 'v8',
      reporter: ['lcov'],
    },
  },
})
