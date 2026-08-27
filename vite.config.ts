// vite.config.ts
import { readFileSync } from 'fs'
import { resolve } from 'path'
import { defineConfig } from 'vitest/config'

import vue from '@vitejs/plugin-vue'
import { schemeParserPlugin } from './scripts/vite-plugin-scheme-parser.mjs'
import { libSourcesPlugin } from './scripts/vite-plugin-lib-sources.mjs'
import { flattenHtmlPlugin } from './scripts/vite-plugin-flatten-html.mjs'
import { devFlatHtmlPlugin } from './scripts/vite-plugin-dev-flat-html.mjs'
import { devServerConfigPlugin } from './scripts/vite-plugin-dev-server-config.mjs'

// Read the version from package.json directly so it's correct regardless of
// how the build/test runner was launched. `npm_package_version` is only set by
// the npm lifecycle (`npm run ...`), not by IDE test runners or a bare
// `vitest`, so relying on it alone leaves APP_VERSION as 'unknown' there.
const pkg = JSON.parse(
  readFileSync(resolve(__dirname, 'package.json'), 'utf-8'),
) as { version?: string }
const AppVersion = process.env.npm_package_version ?? pkg.version ?? 'unknown'

// The single source of truth for the HTML entry points, so the build input,
// the production flattening (flattenHtmlPlugin), and the dev server's flat
// layout (devFlatHtmlPlugin) all stay in sync. Each app's entry lives next to
// its source; the build flattens them to distinct top-level basenames.
const ideEntry = 'src/app/web/index.html'
const htmlEntries: Record<string, string> = {
  'scamper-docs': 'src/app/docs/docs.html',
  'scamper-ide': ideEntry,
  'scamper-embed': 'src/app/web/embed/embed.html',
  'scamper-search': 'src/app/search/search.html',
}

// `vite --mode server` (i.e. `npm run dev:memory`) runs the front end against the
// back end in `server/`. Everything below is off in the default `npm run dev`,
// which knows nothing of a server and stays on local storage.
//
// The dev server proxies the API rather than letting the browser call
// localhost:3000 directly, so a dev checkout is single-origin exactly as
// production is -- one host serving the static site and /api alike. That keeps
// cookies, SameSite, and CORS from behaving one way in dev and another in
// production, which is the failure this whole arrangement exists to avoid.
const API_PREFIX = '/api'
const devServerPort = process.env.SCAMPER_SERVER_PORT ?? '3000'

export default defineConfig(({ mode }) => ({
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
    // Vite drops falsy entries, so this is "only in `--mode server`".
    mode === 'server' && devServerConfigPlugin(`${API_PREFIX}/v1`),
    vue(),
    flattenHtmlPlugin(),
  ],

  server:
    mode === 'server'
      ? {
          proxy: {
            [API_PREFIX]: {
              target: `http://localhost:${devServerPort}`,
              // Keep the browser's Host header: the back end sees the request
              // as same-origin, which is what it will be in production.
              changeOrigin: false,
            },
          },
        }
      : {},

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
}))
