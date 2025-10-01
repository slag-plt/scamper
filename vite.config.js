// vite.config.js
import { resolve } from 'path'
import { defineConfig } from 'vite'

import { lezer } from "@lezer/generator/rollup"

const AppVersion = process.env.npm_package_version

export default defineConfig({
  build: {
    rollupOptions: {
      input: {
        'scamper-docs': resolve(__dirname, 'docs.html'),
        'scamper-ide': resolve(__dirname, 'index.html'),
        'scamper-runner': resolve(__dirname, 'runner.html'),
        'scamper-web': resolve(__dirname, 'web.html'),
      },
      output: {
        entryFileNames: `assets/[name]-${AppVersion}.js`,
        chunkFileNames: `assets/[name]-${AppVersion}.js`,
        assetFileNames: `assets/[name]-${AppVersion}.[ext]`
      }
    },
  },

  plugins: [
    lezer(),
  ],

  define: {
    'APP_VERSION': JSON.stringify(AppVersion)
  },

  test: {
    environment: 'jsdom',
    coverage: {
      provider: 'v8',
    },
  },
})