import { readFileSync } from 'node:fs'
import { basename, resolve } from 'node:path'
import { describe, expect, test } from 'vitest'

import {
  EMBED_ALIAS,
  EMBED_STEM,
  embedBundleName,
} from '../../scripts/embed-bundle-name.mjs'
import { AppVersion, htmlEntries } from '../../vite.config'

// Regression test for the embed bundle being confused with a chunk of the site
// build. `npm run build` produces two files from the same entry point:
//
//   dist/scamper-embed-<version>.js  one self-contained file, what a reading
//                                    on another site includes
//   dist/assets/<key>-<version>.js   a chunk that imports three siblings and
//                                    works only inside the deployment
//
// The site build's key was `scamper-embed`, so the second was emitted as
// `assets/scamper-embed-<version>.js`. Someone embedding Scamper went looking,
// found that one, and reported the bundle as broken because it referenced
// `assets/` -- which it does, correctly, for the page it belongs to.
//
// Renaming it is the whole fix, so this is what keeps it renamed.
//
// #704 then made the bundle itself versioned, so the two names differ only by
// directory. That is why the stem comes from scripts/embed-bundle-name.mjs rather
// than being picked apart from a filename here, and why this guard matters more
// than it did when the bundle was simply `scamper-embed.js`.
//
// The config is read as text rather than imported: it imports `./vite.config.ts`
// with the extension #637 requires, which the test project cannot resolve.
// test/regressions/vite-config-native-loader.test.ts reads it the same way.

const CONFIG = resolve(import.meta.dirname, '../../vite.config.embed.ts')

describe('the embeddable bundle has a name nothing else answers to', () => {
  test('no site-build chunk is named after it', () => {
    const colliding = Object.keys(htmlEntries).filter(
      (key) => key === EMBED_STEM || key.startsWith(`${EMBED_STEM}-`),
    )
    expect(
      colliding,
      'these entry keys become assets/<key>-<version>.js, which reads as a ' +
        `build of ${embedBundleName('<version>')} and is not one`,
    ).toEqual([])
  })

  test('the bundle carries its version, so the site can keep them side by side', () => {
    // #704: the version is in the filename rather than a directory, which is what
    // lets scripts/compose-preview-site hold every release's bundle at the root.
    expect(embedBundleName(AppVersion)).toBe(`${EMBED_STEM}-${AppVersion}.js`)
    expect(embedBundleName(AppVersion)).not.toBe(EMBED_ALIAS)
  })

  test('the build names its output from that one place', () => {
    // So the name cannot drift from what this test and the shim plugin assume.
    const source = readFileSync(CONFIG, 'utf-8')
    expect(source).toContain("from './scripts/embed-bundle-name.mjs'")
    expect(source).toMatch(/fileName:\s*\(\)\s*=>\s*embedBundleName\(AppVersion\)/)
  })

  test('the name it used to have is still written beside it', () => {
    // scripts/vite-plugin-embed-shim.mjs writes it, so samples/reading.html and a
    // self-hosted `<host>/<version>/scamper-embed.js` -- the form
    // docs/embedding.md documents -- go on resolving.
    expect(basename(EMBED_ALIAS, '.js')).toBe(EMBED_STEM)
    expect(readFileSync(CONFIG, 'utf-8')).toContain('embedShimPlugin({')
  })

  test('the demonstration page is still an entry point', () => {
    // The rename must not have dropped the page on the way past: it is what
    // test/apps/web/embed.browser.test.ts drives, and `npm run dev` serves it
    // at /embed.html. flattenHtmlPlugin names the output from the path's
    // basename rather than the key, so the URL does not move.
    const paths = Object.values(htmlEntries)
    expect(paths).toContain('src/app/web/embed/embed.html')
    expect(paths.map((p) => basename(p))).toContain('embed.html')
  })
})
