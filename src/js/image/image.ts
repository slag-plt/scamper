import * as L from '../../lpm'
import { extensionOf } from '../../fs/fs.js'
import { context2d } from './context.js'

/***** Image loading **********************************************************/

export interface ReactiveImageFile extends L.Struct {
  [L.structKind]: 'reactive-image-file',
  callback: L.ScamperFn,
  [L.runField]: L.RunHandle
}

export function image_withImageFile(callback: L.ScamperFn): ReactiveImageFile {
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-image-file',
    callback,
    // See prelude_withFileChooser: the run is captured while the program is
    // stepping, because the renderer mounts too late to resolve it (#397).
    [L.runField]: L.currentRun()
  }
}

export function image_isReactiveImageFile (v: L.Value): boolean {
  return L.isStructKind(v, 'reactive-image-file')
}

/**
 * The image formats a browser decodes, and the type each is handed to it as.
 *
 * This is `src/fs`'s IMAGE_EXTENSIONS plus `.svg`, which that list omits
 * because it answers a different question -- whether the IDE can show the file
 * in place of the editor -- and an SVG is text a student may want to edit.
 * `.tif` is in neither: no browser but Safari decodes one.
 */
const IMAGE_MIME_TYPES = new Map<string, string>([
  ['png', 'image/png'],
  ['jpg', 'image/jpeg'],
  ['jpeg', 'image/jpeg'],
  ['gif', 'image/gif'],
  ['webp', 'image/webp'],
  ['bmp', 'image/bmp'],
  ['ico', 'image/x-icon'],
  ['avif', 'image/avif'],
  ['svg', 'image/svg+xml'],
])

/** The subset of the above a canvas can be encoded back into (`toBlob`). */
const SAVEABLE_MIME_TYPES = new Map(
  [...IMAGE_MIME_TYPES].filter(([extension]) =>
    ['png', 'jpg', 'jpeg', 'webp'].includes(extension),
  ),
)

/** The formats each half reads or writes, as an error message names them. */
const READABLE_FORMATS = formatList(IMAGE_MIME_TYPES)
const SAVEABLE_FORMATS = formatList(SAVEABLE_MIME_TYPES)

function formatList(types: Map<string, string>): string {
  return [...types.keys()].map((extension) => `.${extension}`).join(', ')
}

/**
 * @returns the type `filename`'s extension names it as
 * @throws ScamperError if it names no image format Scamper can read
 */
function imageMimeTypeOf(filename: string): string {
  const type = IMAGE_MIME_TYPES.get(extensionOf(filename))
  if (type === undefined) {
    throw new L.ScamperError(
      'Runtime',
      `Cannot load "${filename}" as an image: its name must end in one of ${READABLE_FORMATS}`,
    )
  }
  return type
}

/**
 * @returns the type `filename`'s extension names it as
 * @throws ScamperError if it is not one a canvas can be encoded into. Checked
 *         rather than left to `toBlob`, which quietly falls back to PNG for a
 *         type it cannot write -- and PNG bytes under the name `out.gif` is a
 *         file nothing will open.
 */
function savedImageMimeTypeOf(filename: string): string {
  const type = SAVEABLE_MIME_TYPES.get(extensionOf(filename))
  if (type === undefined) {
    throw new L.ScamperError(
      'Runtime',
      `Cannot save "${filename}": an image can be saved as ${SAVEABLE_FORMATS}`,
    )
  }
  return type
}

/**
 * Loads `url` into an image element.
 *
 * @param failure what to say if the browser cannot load it, which depends on
 *        where the URL came from
 */
function loadImage(url: string, failure: string): Promise<HTMLImageElement> {
  return new Promise((resolve, reject) => {
    const img = new Image()
    img.onload = () => { resolve(img) }
    img.onerror = () => { reject(new L.ScamperError('Runtime', failure)) }
    img.src = url
  })
}

/** Draws `img` onto a new canvas of its own size, the value Scamper hands back. */
function imageToCanvas(img: HTMLImageElement): HTMLCanvasElement {
  const canvas = document.createElement('canvas')
  canvas.width = img.width
  canvas.height = img.height
  context2d(canvas).drawImage(img, 0, 0)
  return canvas
}

// N.B., suspends the current fiber to load `url` into a canvas asynchronously
// and resumes with that canvas (see SuspendSignal / Scheduler `block-on`). Used
// by the Scheme `with-image-from-url` wrapper -- a JS function can no longer call
// the user's callback. A failed load (or a cross-origin taint) rejects,
// surfacing as a runtime error catchable by with-handler.
export function image_blockOnFetchImage(url: string): L.Value {
  throw new L.SuspendSignal(async () => {
    const canvas = imageToCanvas(
      await loadImage(url, `Failed to load image from "${url}"`),
    )
    // Trigger the cross-origin taint check now, so it surfaces as a clean
    // runtime error rather than deep inside a later pixel operation.
    if (canvas.width > 0 && canvas.height > 0) {
      try {
        context2d(canvas).getImageData(0, 0, 1, 1)
      } catch (e) {
        if (e instanceof DOMException && e.name === 'SecurityError') {
          throw new L.ScamperError(
            'Runtime',
            'cannot manipulate images from domains other than scamper.cs.grinnell.edu',
          )
        }
        throw e
      }
    }
    return canvas
  })
}

/**
 * Reads the image stored in `filename` and resumes with it as a canvas (#452).
 * The `image` counterpart to `file->string`: a blocking whole-file read, so a
 * student writes `(image-load "cat.png")` rather than a callback.
 *
 * N.B., `src/fs` is imported lazily *inside* the action, never at module load:
 * this module is pulled in during test setup (library registration), and
 * eagerly importing it there would grab the real OPFS out from under tests that
 * mock it. Same reasoning as src/js/file/index.ts. (`src/fs/fs.ts` at the top of
 * this file is a different matter -- it is the pure contract, and reaches no
 * storage.)
 */
export function image_imageLoad(filename: string): L.Value {
  throw new L.SuspendSignal(async () => {
    // Before the read, so a name that is not an image says so rather than
    // failing later as an undecodable file.
    const type = imageMimeTypeOf(filename)
    const { getFS } = await import('../../fs')
    const fs = getFS()
    if (!(await fs.fileExists(filename))) {
      throw new L.ScamperError('Runtime', `File "${filename}" does not exist`)
    }
    let bytes
    try {
      bytes = await fs.loadBytes(filename)
    } catch (e) {
      // A ScamperError from the host is already worded for a student and says
      // something more specific than this can (e.g. a name reaching outside the
      // working directory, #340), so it passes through untouched.
      if (e instanceof L.ScamperError) { throw e }
      throw new L.ScamperError('Runtime', `Could not read the file "${filename}"`)
    }
    // The type comes from the name: a blob URL is served with its blob's own
    // type, and an empty one leaves the decoder to guess.
    const url = URL.createObjectURL(new Blob([bytes], { type }))
    try {
      return imageToCanvas(
        await loadImage(url, `Could not read "${filename}" as an image`),
      )
    } finally {
      // On both paths -- the image has finished decoding by the time its load
      // resolves, so nothing still needs the URL.
      URL.revokeObjectURL(url)
    }
  })
}

/** Writes `canvas` to `filename`, encoded as its name says. See image_imageLoad. */
export function image_imageSave(
  canvas: HTMLCanvasElement, filename: string,
): L.Value {
  throw new L.SuspendSignal(async () => {
    const type = savedImageMimeTypeOf(filename)
    const blob = await new Promise<Blob | null>((resolve) => {
      canvas.toBlob(resolve, type)
    })
    if (blob === null) {
      throw new L.ScamperError(
        'Runtime',
        `Could not encode the image to save it to "${filename}"`,
      )
    }
    const { getFS } = await import('../../fs')
    try {
      await getFS().saveBytes(filename, new Uint8Array(await blob.arrayBuffer()))
    } catch (e) {
      if (e instanceof L.ScamperError) { throw e }
      throw new L.ScamperError(
        'Runtime',
        `Could not write the image to "${filename}"`,
      )
    }
    return undefined
  })
}
