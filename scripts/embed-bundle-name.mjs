// The reading-widget bundle's filename, in one place.
//
// The version is in the *filename* since #704, so the GitHub Pages site can keep
// every release's bundle at its root rather than one per directory (see
// scripts/compose-preview-site). Its stem stays `scamper-embed`, which nothing
// else in either Vite build answers to -- a versioned name is exactly the shape a
// site-build chunk takes, and someone once embedded one of those by mistake
// (#631). test/regressions/embed-bundle-name-is-unique.test.ts keeps it unique.
//
// A module of its own so vite.config.embed.ts, the shim plugin, and that test all
// name the same string. Plain `.mjs` so the test tree can import it without
// pulling a Vite config -- and its `.ts`-suffixed imports, which #637 requires --
// into the test project.

/** The bundle's filename stem, without the version or the extension. */
export const EMBED_STEM = 'scamper-embed'

/**
 * The unversioned name the bundle used to have. Written beside the real one, so
 * a self-hosted `<host>/<version>/scamper-embed.js` and samples/reading.html go
 * on resolving.
 */
export const EMBED_ALIAS = `${EMBED_STEM}.js`

/**
 * @param {string} version
 * @returns {string} what the embed build names its output for that version
 */
export function embedBundleName(version) {
  return `${EMBED_STEM}-${version}.js`
}
