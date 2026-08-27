import * as L from '../../lpm'
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

// N.B., suspends the current fiber to load `url` into a canvas asynchronously
// and resumes with that canvas (see SuspendSignal / Scheduler `block-on`). Used
// by the Scheme `with-image-from-url` wrapper -- a JS function can no longer call
// the user's callback. A failed load (or a cross-origin taint) rejects,
// surfacing as a runtime error catchable by with-handler.
export function image_blockOnFetchImage(url: string): L.Value {
  throw new L.SuspendSignal(
    () =>
      new Promise<L.Value>((resolve, reject) => {
        const img = new Image()
        img.onload = () => {
          const canvas = document.createElement('canvas')
          canvas.width = img.width
          canvas.height = img.height
          const ctx = context2d(canvas)
          ctx.drawImage(img, 0, 0)
          // Trigger the cross-origin taint check now, so it surfaces as a clean
          // runtime error rather than deep inside a later pixel operation.
          if (canvas.width > 0 && canvas.height > 0) {
            try {
              ctx.getImageData(0, 0, 1, 1)
            } catch (e) {
              if (e instanceof DOMException && e.name === 'SecurityError') {
                reject(
                  new L.ScamperError(
                    'Runtime',
                    'cannot manipulate images from domains other than scamper.cs.grinnell.edu',
                  ),
                )
                return
              }
              reject(e as Error)
              return
            }
          }
          resolve(canvas)
        }
        img.onerror = () => {
          reject(
            new L.ScamperError('Runtime', `Failed to load image from "${url}"`),
          )
        }
        img.src = url
      }),
  )
}
