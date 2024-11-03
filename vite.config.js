// vite.config.js
import { resolve } from 'path'
import { defineConfig } from 'vite'

import { lezer } from "@lezer/generator/rollup"
// import { nodeResolve } from '@rollup/plugin-node-resolve'
// import commonjs from '@rollup/plugin-commonjs'

export default defineConfig({
  build: {
    rollupOptions: {
      input: {
        fileChooser: resolve(__dirname, 'index.html'),
        ide: resolve(__dirname, 'ide.html'),
        runner: resolve(__dirname, 'runner.html'),
      },
    },
  },

  plugins: [
    lezer(),
    // nodeResolve(),
    // commonjs(),
  ],

  define: {
    'APP_VERSION': JSON.stringify(process.env.npm_package_version),
  }
})