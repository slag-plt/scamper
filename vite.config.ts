// vite.config.ts
import { resolve } from 'path'
import { defineConfig } from 'vitest/config'

import vue from '@vitejs/plugin-vue'
import { schemeParserPlugin } from './scripts/vite-plugin-scheme-parser.mjs'
import { libSourcesPlugin } from './scripts/vite-plugin-lib-sources.mjs'
import { flattenHtmlPlugin } from './scripts/vite-plugin-flatten-html.mjs'
import { devRootRedirectPlugin } from './scripts/vite-plugin-dev-root-redirect.mjs'

const AppVersion = process.env.npm_package_version ?? 'unknown'

export default defineConfig({
  build: {
    rolldownOptions: {
      input: {
        'scamper-docs': resolve(__dirname, 'src/app/docs/docs.html'),
        'scamper-ide': resolve(__dirname, 'src/app/web/index.html'),
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
    devRootRedirectPlugin(),
    vue(),
    flattenHtmlPlugin(),
  ],

  define: {
    APP_VERSION: JSON.stringify(AppVersion),
  },

  test: {
    environment: 'jsdom',
    setupFiles: './test/setup.ts',
    coverage: {
      provider: 'v8',
      reporter: ['lcov'],
    },
  },
})
