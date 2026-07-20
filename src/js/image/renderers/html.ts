import * as L from '../../../lpm'
import HtmlRenderer from '../../../lpm/renderers/html.js'
import { Rgb, Hsv, image_isRgb, image_isHsv, image_rgbPseudoComplement, image_rgbToString, image_hsvToRgb, image_hsvToString } from '../color.js'
import { Drawing, image_drawingQ, image_renderer } from '../drawing.js'
import { ReactiveImageFile, image_isReactiveImageFile } from '../image.js'

/***** Colors ******************************************************************/

function renderRgb (rgb: Rgb): HTMLElement {
  const div = document.createElement('div')
  const textColor = image_rgbPseudoComplement(rgb)
  div.style.color = image_rgbToString(textColor)
  div.style.backgroundColor = image_rgbToString(rgb)
  div.style.width = 'fit-content'
  div.style.border = '1px solid black'
  div.style.padding = '0.25em'
  div.textContent = image_rgbToString(rgb)
  return div
}

HtmlRenderer.registerCustomRenderer(image_isRgb, (v: any) => renderRgb(v as Rgb))

function renderHsv (hsv: Hsv): HTMLElement {
  const div = document.createElement('div')
  const rgb = image_hsvToRgb(hsv)
  const textColor = image_rgbPseudoComplement(rgb)
  div.style.color = image_rgbToString(textColor)
  div.style.backgroundColor = image_rgbToString(rgb)
  div.style.width = 'fit-content'
  div.style.border = '1px solid black'
  div.style.padding = '0.25em'
  div.textContent = image_hsvToString(hsv)
  return div
}

HtmlRenderer.registerCustomRenderer(image_isHsv, (v: any) => renderHsv(v as Hsv))

/***** Drawings ****************************************************************/

HtmlRenderer.registerCustomRenderer(image_drawingQ, (v: any) => image_renderer(v as Drawing))

/***** Reactive image files *****************************************************/

function render (rif: ReactiveImageFile): HTMLElement {
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
          outp.innerHTML = ''
          const canvas = document.createElement('canvas')
          const ctx = canvas.getContext('2d')
          if (ctx) {
            canvas.width = img.width
            canvas.height = img.height
            ctx.drawImage(img, 0, 0)
          }
          try {
            const v = L.callScamperFn(rif.callback, canvas)
            outp.appendChild(HtmlRenderer.render(v))
          } catch (e) {
            outp.appendChild(HtmlRenderer.render(e as L.ScamperError))
          }
        }
        img.src = e.target.result as string;
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
