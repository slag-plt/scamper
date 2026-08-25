import * as L from '../../../lpm'
import HtmlRenderer from '../../../lpm/renderers/html.js'
import { Rgb, Hsv, color_isRgb, color_isHsv, color_rgbPseudoComplement, color_rgbToString, color_hsvToRgb, color_hsvToString } from '../color.js'
import { Drawing, drawing_drawingQ, drawing_renderer } from '../drawing.js'
import { ReactiveImageFile, image_isReactiveImageFile } from '../image.js'

/***** Colors ******************************************************************/

function renderRgb (rgb: Rgb): HTMLElement {
  const div = document.createElement('div')
  const textColor = color_rgbPseudoComplement(rgb)
  div.style.color = color_rgbToString(textColor)
  div.style.backgroundColor = color_rgbToString(rgb)
  div.style.width = 'fit-content'
  div.style.border = '1px solid var(--border)'
  div.style.padding = '0.25em'
  div.textContent = color_rgbToString(rgb)
  return div
}

HtmlRenderer.registerCustomRenderer(color_isRgb, (v: any) => renderRgb(v as Rgb))

function renderHsv (hsv: Hsv): HTMLElement {
  const div = document.createElement('div')
  const rgb = color_hsvToRgb(hsv)
  const textColor = color_rgbPseudoComplement(rgb)
  div.style.color = color_rgbToString(textColor)
  div.style.backgroundColor = color_rgbToString(rgb)
  div.style.width = 'fit-content'
  div.style.border = '1px solid var(--border)'
  div.style.padding = '0.25em'
  div.textContent = color_hsvToString(hsv)
  return div
}

HtmlRenderer.registerCustomRenderer(color_isHsv, (v: any) => renderHsv(v as Hsv))

/***** Drawings ****************************************************************/

HtmlRenderer.registerCustomRenderer(drawing_drawingQ, (v: any) => drawing_renderer(v as Drawing))

/***** Reactive image files *****************************************************/

function render (rif: ReactiveImageFile): HTMLElement {
  // Rendering happens as the program's output is emitted, i.e. while it is
  // stepping, so this captures the run that produced the value. The callback
  // below fires from a FileReader long afterwards (#375).
  const run = L.currentRun()
  const ret = document.createElement('div')
  const inp = document.createElement('input')
  const outp = document.createElement('div')

  inp.type = 'file'
  inp.accept = 'image/*'
  inp.addEventListener('change', () => {
    const reader = new FileReader()
    reader.onload = (e) => {
      if (e !== null && e.target !== null) {
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
          run.spawn(rif.callback, [canvas], (r) => {
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

HtmlRenderer.registerCustomRenderer(image_isReactiveImageFile, (v: any) => render(v as ReactiveImageFile))
