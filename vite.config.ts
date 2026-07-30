// vite.config.ts
import { resolve } from 'path'
import { defineConfig } from 'vitest/config'

import vue from '@vitejs/plugin-vue'
import { schemeParserPlugin } from './scripts/vite-plugin-scheme-parser.mjs'
import { libSourcesPlugin } from './scripts/vite-plugin-lib-sources.mjs'
import { flattenHtmlPlugin } from './scripts/vite-plugin-flatten-html.mjs'
import { devRootRedirectPlugin } from './scripts/vite-plugin-dev-root-redirect.mjs'

const AppVersion = process.env.npm_package_version ?? 'unknown'

// The single source of truth for the IDE's HTML entry path, so
// devRootRedirectPlugin's dev-server redirect target can't drift out of
// sync with the build's own input mapping below.
const ideEntry = 'src/app/web/index.html'

export default defineConfig({
  build: {
    rolldownOptions: {
      input: {
        'scamper-docs': resolve(__dirname, 'src/app/docs/docs.html'),
        'scamper-ide': resolve(__dirname, ideEntry),
        'scamper-runner': resolve(__dirname, 'src/app/web/runner.html'),
        'scamper-web': resolve(__dirname, 'src/app/web/web.html'),
        'scamper-search': resolve(__dirname, 'src/app/search/search.html'),
      },
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
    devRootRedirectPlugin(`/${ideEntry}`),
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
    // jsdom's stubbed-out canvas support.
    exclude: ['**/node_modules/**', '**/.git/**', '**/*.browser.test.ts'],
    coverage: {
      provider: 'v8',
      reporter: ['lcov'],
    },
  },
})
