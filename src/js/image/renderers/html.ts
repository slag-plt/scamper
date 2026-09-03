import * as L from '../../../lpm'
import HtmlRenderer from '../../../lpm/renderers/html.js'
import { Rgb, Hsv, color_isRgb, color_isHsv, color_rgbToString, color_hsvToRgb, color_hsvToString } from '../color.js'
import { swatchFill, swatchInk } from './swatch.js'
import { Drawing, drawing_drawingQ, drawing_renderer } from '../drawing.js'
import { ReactiveImageFile, image_isReactiveImageFile } from '../image.js'

/***** Colors ******************************************************************/

/**
 * A swatch of `color` with `label` written across it, in whichever of black
 * or white reads better over the swatch (see `swatch.ts`).
 */
function renderSwatch (color: Rgb, label: string): HTMLElement {
  const div = document.createElement('div')
  div.style.color = swatchInk(color)
  div.style.backgroundColor = color_rgbToString(swatchFill(color))
  div.style.width = 'fit-content'
  div.style.border = '1px solid var(--border)'
  div.style.padding = '0.25em'
  div.textContent = label
  return div
}

HtmlRenderer.registerCustomRenderer(color_isRgb, (v: L.Value) => {
  const rgb = v as Rgb
  return renderSwatch(rgb, color_rgbToString(rgb))
})

HtmlRenderer.registerCustomRenderer(color_isHsv, (v: L.Value) => {
  const hsv = v as Hsv
  return renderSwatch(color_hsvToRgb(hsv), color_hsvToString(hsv))
})

/***** Drawings ****************************************************************/

HtmlRenderer.registerCustomRenderer(drawing_drawingQ, (v: L.Value) => drawing_renderer(v as Drawing))

/***** Reactive image files *****************************************************/

function render (rif: ReactiveImageFile): HTMLElement {
  // The run comes from the value. Rendering looks like it happens during a
  // step, but the scheduler clears `steppingTaskId` before emitting output, so
  // resolving one here finds the foreground run -- and a reading page has none
  // (#397).
  const ret = document.createElement('div')
  const inp = document.createElement('input')
  const outp = document.createElement('div')

  inp.type = 'file'
  inp.accept = 'image/*'
  inp.addEventListener('change', () => {
    const reader = new FileReader()
    reader.onload = (e) => {
      if (e.target !== null) {
        const img = new Image()
        img.onload = () => {
          const canvas = document.createElement('canvas')
          const ctx = canvas.getContext('2d')
          if (ctx) {
            canvas.width = img.width
            canvas.height = img.height
            ctx.drawImage(img, 0, 0)
          }
          // Run the callback as a fiber (JS can no longer call the closure) and
          // render its result; a callback error surfaces in the output pane.
          rif[L.runField].spawn(rif.callback, [canvas], (r) => {
            outp.innerHTML = ''
            if (r !== null) {
              outp.appendChild(HtmlRenderer.render(r))
            }
          })
        }
        img.src = e.target.result as string
      }
    }
    if (inp.files !== null && inp.files.length > 0) {
      outp.innerText = 'Loading...'
      reader.readAsDataURL(inp.files[0])
    }
  }, false)

  ret.appendChild(inp)
  ret.appendChild(document.createElement('br'))
  ret.appendChild(outp)
  return ret
}

HtmlRenderer.registerCustomRenderer(image_isReactiveImageFile, (v: L.Value) => render(v as ReactiveImageFile))
