import { checkContract, contract } from '../../contract.js'
import * as C from '../../contract.js'
import * as Render from '../../display.js'
import * as L from '../../lpm'
import { rgb } from './color.js'

const imageS: C.Spec = {
  predicate: (v: any) => v instanceof HTMLCanvasElement,
  errorMsg: (actual: any) => `expected an image, received ${L.typeOf(actual)}`
}

/***** Image loading **********************************************************/

export interface ReactiveImageFile extends L.Struct {
  [L.structKind]: 'reactive-image-file',
  callback: L.ScamperFn
}

function withImageFile (callback: L.ScamperFn): ReactiveImageFile {
  checkContract(arguments, contract('with-image-file', [C.func]))  
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-image-file',
    callback
  }
}

function isReactiveImageFile (v: any): boolean {
  return L.isStructKind(v, 'reactive-image-file')
}

function withImageFromUrl (url: string, callback: L.ScamperFn): HTMLElement {
    checkContract(arguments, contract('with-image-from-url', [C.string, C.func]))
    const container = document.createElement('div')
    container.innerHTML = `Loading ${url}...`
    const img = new Image()
    img.onload = () => {
        container.innerHTML = ''
        const canvas = document.createElement('canvas')
        canvas.width = img.width
        canvas.height = img.height
        const ctx = canvas.getContext('2d')!
        ctx.drawImage(img, 0, 0)
        try {
          const v = L.callScamperFn(callback, canvas)
          container.appendChild(Render.renderToHTML(v))
        } catch (e) {
          if (e instanceof DOMException && e.name === 'SecurityError') {
            container.innerHTML = `Failed to load ${url}: cannot manipulate images from domains other than scamper.cs.grinnell.edu`
          } else {
            container.appendChild(Render.renderToHTML(e as L.ScamperError))
          }
        }
    } 
    img.src = url
    return container
}

/***** Per-pixel manipulation *************************************************/

function pixelMap (fn: L.ScamperFn, canvas: HTMLCanvasElement): HTMLCanvasElement {
  checkContract(arguments, contract('pixel-map', [C.func, imageS]))
  const ctx = canvas.getContext('2d')!
  const inpImg = ctx.getImageData(0, 0, canvas.width, canvas.height)!
  const src = inpImg.data

  const outImg = ctx.createImageData(canvas.width, canvas.height)
  const dst = outImg.data
  for (let i = 0; i < src.length; i += 4) {
    const c = L.callScamperFn(fn, rgb(src[i], src[i + 1], src[i+2], src[i+3]))
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

function imageGetPixel (canvas: HTMLCanvasElement, x: number, y: number): L.Struct {
  checkContract(arguments, contract('image-get-pixel', [imageS, C.integer, C.integer]))
  const ctx = canvas.getContext('2d')!
  const img = ctx.getImageData(x, y, 1, 1)
  const data = img.data
  return rgb(data[0], data[1], data[2], data[3])
}

function imageToPixels (canvas: HTMLCanvasElement): L.Struct[] {
  checkContract(arguments, contract('image-to-pixels', [imageS]))
  const ctx = canvas.getContext('2d')!
  const src = ctx.getImageData(0, 0, canvas.width, canvas.height)!.data
  const ret = []
  for (let i = 0; i < src.length; i += 4) {
    ret.push(rgb(src[i], src[i + 1], src[i + 2], src[i + 3]))
  }
  return ret
}

function pixelsToImage (pixels: L.Struct[], width: number, height: number): HTMLCanvasElement {
  checkContract(arguments, contract('pixels-to-image', [C.vector, C.integer, C.integer]))
  const ret = document.createElement('canvas')
  ret.width = width
  ret.height = height
  const ctx = ret.getContext('2d')!
  const outImg = ctx.createImageData(width, height)
  const data = outImg.data
  for (let i = 0; i < pixels.length; i++) {
    const c = pixels[i]
    data[i*4] = c.red
    data[i*4 + 1] = c.green
    data[i*4 + 2] = c.blue
    data[i*4 + 3] = c.alpha
  }
  ctx.putImageData(outImg, 0, 0)
  return ret
}

function canvasSetPixels (canvas: HTMLCanvasElement, pixels: L.Struct[]): void {
  checkContract(arguments, contract('canvas-set-pixels!', [imageS, C.vector]))
  const ctx = canvas.getContext('2d')!
  const outImg = ctx.createImageData(canvas.width, canvas.height)
  const data = outImg.data
  for (let i = 0; i < pixels.length; i++) {
    const c = pixels[i]
    data[i*4] = c.red
    data[i*4 + 1] = c.green
    data[i*4 + 2] = c.blue
    data[i*4 + 3] = c.alpha
  }
  ctx.putImageData(outImg, 0, 0)
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
            const v = L.callScamperFn(rif.callback, canvas)
            outp.appendChild(Render.renderToHTML(v))
          } catch (e) {
            outp.appendChild(Render.renderToHTML(e as L.ScamperError))
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

export const lib: L.Library = new L.Library()

// Image loading
lib.registerValue('with-image-file', withImageFile)
lib.registerValue('with-image-from-url', withImageFromUrl)

// Per-pixel manipulation
lib.registerValue('pixel-map', pixelMap)
lib.registerValue('image-get-pixel', imageGetPixel)
lib.registerValue('image->pixels', imageToPixels)
lib.registerValue('pixels->image', pixelsToImage)
lib.registerValue('canvas-set-pixels!', canvasSetPixels)