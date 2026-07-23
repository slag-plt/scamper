import { basename } from 'path'

// The five HTML entry points live next to their app's source (src/app/web/,
// src/app/docs/, src/app/search/) so they're not sitting oddly at the
// project root, but Vite's build output mirrors each entry's path relative
// to the project root -- e.g. src/app/web/index.html would otherwise land
// at dist/src/app/web/index.html. The deploy scripts (scripts/deploy,
// scripts/update-latest) and the web server rely on a flat dist/ (index.html,
// web.html, runner.html, docs.html, search.html all at the top level), so
// this renames each emitted HTML asset back down to its basename. Each
// entry's basename is currently unique, so no explicit path mapping is
// needed -- but a future entry sharing a basename (e.g. a second
// src/app/<newapp>/index.html) would otherwise flatten to the same dist
// path and silently overwrite another app's output, so we check for and
// reject that instead of letting it happen quietly.
export function flattenHtmlPlugin() {
  return {
    name: 'flatten-html-output',
    enforce: 'post',
    generateBundle(_options, bundle) {
      const seen = new Set()
      for (const asset of Object.values(bundle)) {
        if (!asset.fileName.endsWith('.html')) continue
        const flatName = basename(asset.fileName)
        if (seen.has(flatName)) {
          throw new Error(
            `flattenHtmlPlugin: multiple HTML entries flatten to "${flatName}" -- give each entry a distinct basename.`,
          )
        }
        seen.add(flatName)
        asset.fileName = flatName
      }
    },
  }
}
