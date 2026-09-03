import * as L from '../../lpm'
import { context2d } from './context.js'

/**
 * Turning an image the browser can fetch into the canvas Scamper hands a
 * program. Three places need this -- `image-load`, `with-image-from-url`, and
 * the file chooser `with-image-file` renders -- so it lives here rather than
 * being written out a third time.
 *
 * Alongside `context.ts` and deliberately not re-exported by `index.ts`: these
 * are helpers, not Scamper bindings, and everything `index.ts` exports is
 * flattened into the `js-var` map (see src/js/index.ts).
 */

/**
 * Loads `url` into an image element.
 *
 * @param failure what to say if the browser cannot load it, which depends on
 *        where the URL came from
 */
export function loadImage(
  url: string, failure: string,
): Promise<HTMLImageElement> {
  return new Promise((resolve, reject) => {
    const img = new Image()
    img.onload = () => { resolve(img) }
    img.onerror = () => { reject(new L.ScamperError('Runtime', failure)) }
    img.src = url
  })
}

/** Draws `img` onto a new canvas of its own size, the value Scamper hands back. */
export function imageToCanvas(img: HTMLImageElement): HTMLCanvasElement {
  const canvas = document.createElement('canvas')
  canvas.width = img.width
  canvas.height = img.height
  context2d(canvas).drawImage(img, 0, 0)
  return canvas
}
