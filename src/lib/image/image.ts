import { checkContract, contract } from '../../contract.js'
import * as C from '../../contract.js'
import * as Render from '../../display.js'
import { ScamperError, Value } from '../../lang.js'
import { callFunction } from '../../sem.js'
import { rgb } from './rgb.js'

/***** Image loading **********************************************************/

export interface ReactiveImageFile extends Value.Struct {
  [Value.structKind]: 'reactive-image-file',
  callback: Value.ScamperFn
}

function withImageFile (callback: Value.ScamperFn): ReactiveImageFile {
  checkContract(arguments, contract('with-image-file', [C.func]))  
  return {
    [Value.scamperTag]: 'struct',
    [Value.structKind]: 'reactive-image-file',
    callback
  }
}

function isReactiveImageFile (v: any): boolean {
  return Value.isStructKind(v, 'reactive-image-file')
}

/***** Per-pixel manipulation *************************************************/

function pixelMap (fn: Value.ScamperFn, canvas: HTMLCanvasElement): HTMLCanvasElement {
  const ctx = canvas.getContext('2d')!
  const inpImg = ctx.getImageData(0, 0, canvas.width, canvas.height)!
  const src = inpImg.data


  const outImg = ctx.createImageData(canvas.width, canvas.height)
  const dst = outImg.data
  for (let i = 0; i < src.length; i += 4) {
    const c = callFunction(fn, rgb(src[i], src[i + 1], src[i+2], src[i+3]))
    dst[i] = c.red
    dst[i + 1] = c.green
    dst[i + 2] = c.blue
    dst[i + 3] = c.alpha
  }

  // NOTE: clone the results to a new canvas. Will likely need to implement a
  // mutable version of this function to mitigate the performance hit of
  // processing large images.
  const ret = document.createElement('canvas')
  ret.width = canvas.width
  ret.height = canvas.height
  const retCtx = ret.getContext('2d')!
  retCtx.putImageData(outImg, 0, 0)
  return ret
}

/***** Rendering **************************************************************/

function render(rif: ReactiveImageFile): HTMLElement {
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
          var canvas = document.createElement('canvas')
          const ctx = canvas.getContext('2d')
          if (ctx) {
            canvas.width = img.width
            canvas.height = img.height
            ctx.drawImage(img, 0, 0)
          }
          try {
            const v = callFunction(rif.callback, canvas)
            outp.appendChild(Render.renderToHTML(v))
          } catch (e) {
            outp.appendChild(Render.renderToHTML(e as ScamperError))
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

Render.addCustomWebRenderer(isReactiveImageFile, render)

/***** Exports ****************************************************************/

export const lib: [string, Value.T][] = []

function registerFn (name: string, fn: Function, map: [string, Value.T][]) {
  Value.nameFn(name, fn)
  map.push([name, fn])
}

// Image loading
registerFn('with-image-file', withImageFile, lib)

// Per-pixel manipulation
registerFn('pixel-map', pixelMap, lib)