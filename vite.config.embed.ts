// vite.config.embed.ts
import { resolve } from 'path'
import { defineConfig } from 'vite'

import { AppVersion, sourcePlugins } from './vite.config'
import { inlineCssPlugin } from './scripts/vite-plugin-inline-css.mjs'

// The second half of `npm run build` (see `scripts/build`): the reading widget
// as one self-contained file, `dist/scamper-embed.js`.
//
// The main build emits the same entry point as well, but as a page's worth of
// shared chunks under `assets/` with the stylesheets left to the page -- fine
// for embed.html, which sits in the deployment beside them, useless for the
// readings this exists for, which live on another site entirely and can only
// reasonably be asked to add one <script> tag. So this build inlines the
// dynamic imports (one chunk) and the CSS (no stylesheet to link), leaving a
// file with no URLs of its own and hence no dependence on where it is served
// from -- the `--base` a deploy passes does not reach it, and need not.
//
// A separate config rather than a `--mode` branch in vite.config.ts: a mode
// other than `production` would leave Vite's isProduction false and ship Vue's
// development build. What the two need in common is imported above.
export default defineConfig({
  build: {
    // The main build has already written (and emptied) dist/, including
    // everything in public/; this only adds a file to it.
    outDir: 'dist',
    emptyOutDir: false,
    copyPublicDir: false,
    cssCodeSplit: false,
    // Everything an import resolves to becomes part of the file, however
    // large: a second emitted file is exactly what this build must not have,
    // and inlineCssPlugin fails the build if one appears anyway.
    assetsInlineLimit: () => true,
    lib: {
      entry: resolve(__dirname, 'src/app/web/embed/embed-entry.ts'),
      // An ES module because the entry awaits at the top level. A reading
      // includes it as <script type="module">, which also defers it.
      formats: ['es'],
      fileName: () => 'scamper-embed.js',
    },
    rolldownOptions: {
      // Scamper loads its renderers by dynamic import (see src/scamper.ts),
      // which would otherwise split into a second chunk.
      output: { codeSplitting: false },
    },
  },

  plugins: [
    ...sourcePlugins(),
    inlineCssPlugin({
      // What embed.html links for itself, and what a reading would otherwise
      // have to. The Vue components' own styles follow, from the build.
      baseStyles: [
        resolve(__dirname, 'public/css/scamper-highlight.css'),
        resolve(__dirname, 'public/css/scamper-transcript.css'),
      ],
    }),
  ],

  define: {
    APP_VERSION: JSON.stringify(AppVersion),
    // A library build leaves this to whoever bundles the library, but nobody
    // is going to: the output is the final file a page loads. Undefined, Vue's
    // reads of it are a `process is not defined` on load and no widget renders.
    'process.env.NODE_ENV': JSON.stringify('production'),
  },
})
