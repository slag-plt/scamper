import * as L from '../../../lpm'
import HtmlRenderer from '../../../lpm/renderers/html.js'
import { Rgb, Hsv, isRgb, isHsv, rgbPseudoComplement, rgbToString, hsvToRgb, hsvToString } from '../color.js'
import { Drawing, drawingQ, renderer } from '../drawing.js'
import { ReactiveImageFile, isReactiveImageFile } from '../image.js'

/***** Colors ******************************************************************/

function renderRgb (rgb: Rgb): HTMLElement {
  const div = document.createElement('div')
  const textColor = rgbPseudoComplement(rgb)
  div.style.color = rgbToString(textColor)
  div.style.backgroundColor = rgbToString(rgb)
  div.style.width = 'fit-content'
  div.style.border = '1px solid black'
  div.style.padding = '0.25em'
  div.textContent = rgbToString(rgb)
  return div
}

HtmlRenderer.registerCustomRenderer(isRgb, (v: any) => renderRgb(v as Rgb))

function renderHsv (hsv: Hsv): HTMLElement {
  const div = document.createElement('div')
  const rgb = hsvToRgb(hsv)
  const textColor = rgbPseudoComplement(rgb)
  div.style.color = rgbToString(textColor)
  div.style.backgroundColor = rgbToString(rgb)
  div.style.width = 'fit-content'
  div.style.border = '1px solid black'
  div.style.padding = '0.25em'
  div.textContent = hsvToString(hsv)
  return div
}

HtmlRenderer.registerCustomRenderer(isHsv, (v: any) => renderHsv(v as Hsv))

/***** Drawings ****************************************************************/

HtmlRenderer.registerCustomRenderer(drawingQ, (v: any) => renderer(v as Drawing))

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

HtmlRenderer.registerCustomRenderer(isReactiveImageFile, (v: any) => render(v as ReactiveImageFile))
