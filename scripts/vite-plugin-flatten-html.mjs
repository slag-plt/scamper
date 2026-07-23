import { basename } from 'path'

// The five HTML entry points live next to their app's source (src/app/web/,
// src/app/docs/, src/app/search/) so they're not sitting oddly at the
// project root, but Vite's build output mirrors each entry's path relative
// to the project root -- e.g. src/app/web/index.html would otherwise land
// at dist/src/app/web/index.html. The deploy scripts (scripts/deploy,
// scripts/update-latest) and the web server rely on a flat dist/ (index.html,
// web.html, runner.html, docs.html, search.html all at the top level), so
// this renames each emitted HTML asset back down to its basename. Each
// entry's basename is already unique, so no explicit path mapping is needed.
export function flattenHtmlPlugin() {
  return {
    name: 'flatten-html-output',
    enforce: 'post',
    generateBundle(_options, bundle) {
      for (const asset of Object.values(bundle)) {
        if (!asset.fileName.endsWith('.html')) continue
        asset.fileName = basename(asset.fileName)
      }
    },
  }
}
