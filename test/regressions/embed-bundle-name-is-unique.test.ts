import { basename } from 'path'
import { describe, expect, test } from 'vitest'

import { htmlEntries } from '../../vite.config'

// Regression test for the embed bundle being confused with a chunk of the site
// build. `npm run build` produces two files from the same entry point:
//
//   dist/scamper-embed.js            one self-contained file, what a reading
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

/** The bundle a reading includes, from vite.config.embed.ts's `fileName`. */
const EMBED_BUNDLE = 'scamper-embed.js'

describe('the embeddable bundle has a name nothing else answers to', () => {
  test('no site-build chunk is named after it', () => {
    const stem = basename(EMBED_BUNDLE, '.js')
    const colliding = Object.keys(htmlEntries).filter(
      (key) => key === stem || key.startsWith(`${stem}-`),
    )
    expect(
      colliding,
      'these entry keys become assets/<key>-<version>.js, which reads as a ' +
        `build of ${EMBED_BUNDLE} and is not one`,
    ).toEqual([])
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
