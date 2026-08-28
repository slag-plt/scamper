import { readFileSync, rmSync, writeFileSync } from 'fs'
import { join } from 'path'

// The reading-widget bundle (vite.config.embed.ts) is meant to be *one* file: a
// reading on some other site adds a single <script> and gets working
// transcripts. That rules out the two things a build normally emits alongside
// the JS -- a stylesheet, and any asset a stylesheet or component refers to --
// so this folds every stylesheet the build produced into the chunk itself, as a
// <style> the bundle appends on load.
//
// It also enforces the "one file" claim: anything left besides the single JS
// chunk means the output is no longer self-contained, and that is a build error
// rather than a page that half-works once it is deployed.

/**
 * @param {string} css the stylesheet text to append on load
 * @returns {string} the IIFE that appends it
 */
function injector(css) {
  return `(function(){
  if (typeof document === 'undefined') { return }
  const style = document.createElement('style')
  style.dataset.scamperEmbed = ''
  style.textContent = ${JSON.stringify(css)}
  document.head.appendChild(style)
})();
`
}

/**
 * @param {{ source: string | Uint8Array }} asset
 * @returns {string} its contents as text, emitted as a string or as bytes.
 */
function assetText(asset) {
  return typeof asset.source === 'string'
    ? asset.source
    : Buffer.from(asset.source).toString('utf-8')
}

/**
 * Inlines the build's CSS into its single JS chunk.
 *
 * @param {{ baseStyles?: string[] }} [opts] `baseStyles` are paths of
 *        stylesheets a page would otherwise `<link>` for itself, prepended in
 *        order ahead of the CSS the build emitted.
 * @returns {import('vite').Plugin}
 */
export function inlineCssPlugin({ baseStyles = [] } = {}) {
  return {
    name: 'inline-css',
    enforce: 'post',
    // Rewriting what was written, rather than the bundle Rolldown hands to
    // generateBundle: entries there can be read and renamed but not removed --
    // a `delete` on one is silently ignored -- and a stylesheet that cannot be
    // removed is a stylesheet the page still has to link.
    writeBundle(options, bundle) {
      const entries = Object.values(bundle)
      const styles = entries.filter((e) => e.fileName.endsWith('.css'))
      const chunks = entries.filter((e) => e.type === 'chunk')
      if (
        chunks.length !== 1 ||
        styles.length + chunks.length !== entries.length
      ) {
        const names = entries.map((e) => e.fileName).join(', ')
        throw new Error(
          `inlineCssPlugin: expected one JS chunk and stylesheets, got ${names} --` +
            ' the bundle is no longer a single self-contained file.',
        )
      }

      const css = [
        ...baseStyles.map((path) => readFileSync(path, 'utf-8')),
        ...styles.map(assetText),
      ].join('\n')

      const chunkPath = join(options.dir, chunks[0].fileName)
      writeFileSync(chunkPath, injector(css) + readFileSync(chunkPath, 'utf-8'))
      for (const style of styles) {
        rmSync(join(options.dir, style.fileName))
      }
    },
  }
}
