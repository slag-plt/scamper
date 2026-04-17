import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import HtmlRenderer from '../../lpm/renderers/html.js'
import * as L from '../../lpm'
import { rgb } from './color.js'

const imageS: C.Spec = {
  predicate: (v: L.Value) => v instanceof HTMLCanvasElement,
  errorMsg: (actual: L.Value) => `expected an image, received ${L.typeOf(actual)}`
}

/***** Image loading **********************************************************/

export interface ReactiveImageFile extends L.Struct {
  [L.structKind]: 'reactive-image-file',
  callback: L.ScamperFn
}

function withImageFile (callback: L.ScamperFn): ReactiveImageFile {
  checkContract([callback], contract('with-image-file', [C.func]))  
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-image-file',
    callback
  }
}

function isReactiveImageFile (v: L.Value): boolean {
  return L.isStructKind(v, 'reactive-image-file')
}

function withImageFromUrl (url: string, callback: L.ScamperFn): HTMLElement {
    checkContract([url, callback], contract('with-image-from-url', [C.string, C.func]))
    const container = document.createElement('div')
    container.innerHTML = `Loading ${url}...`
    const img = new Image()
    img.onload = () => {
        container.innerHTML = ''
        const canvas = document.createElement('canvas')
        canvas.width = img.width
        canvas.height = img.height
        const ctx = canvas.getContext('2d')
        if (ctx === null) {
          throw new L.ScamperError('Runtime', 'could not get 2d rendering context')
        }
        ctx.drawImage(img, 0, 0)
        try {
          const v = L.callScamperFn(callback, canvas)
          container.appendChild(HtmlRenderer.render(v))
        } catch (e) {
          if (e instanceof DOMException && e.name === 'SecurityError') {
            container.innerHTML = `Failed to load ${url}: cannot manipulate images from domains other than scamper.cs.grinnell.edu`
          } else {
            container.appendChild(HtmlRenderer.render(e as L.ScamperError))
          }
        }
    } 
    img.src = url
    return container
}

/***** Per-pixel manipulation *************************************************/

function pixelMap (fn: L.ScamperFn, canvas: HTMLCanvasElement): HTMLCanvasElement {
  checkContract([fn, canvas], contract('pixel-map', [C.func, imageS]))
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new L.ScamperError('Runtime', 'could not get 2d rendering context')
  }
  const inpImg = ctx.getImageData(0, 0, canvas.width, canvas.height)
  const src = inpImg.data

  const outImg = ctx.createImageData(canvas.width, canvas.height)
  const dst = outImg.data
  for (let i = 0; i < src.length; i += 4) {
    const c = L.callScamperFn(fn, rgb(src[i], src[i + 1], src[i + 2], src[i + 3]))
    if (!L.isStructKind(c, 'rgb')) {
      throw new L.ScamperError('Runtime', `pixel-map expected an rgb value, received ${L.typeOf(c)}`)
    }
    const color = c as ReturnType<typeof rgb>
    dst[i] = color.red
    dst[i + 1] = color.green
    dst[i + 2] = color.blue
    dst[i + 3] = color.alpha
  }

  // NOTE: clone the results to a new canvas. Will likely need to implement a
  // mutable version of this function to mitigate the performance hit of
  // processing large images.
  const ret = document.createElement('canvas')
  ret.width = canvas.width
  ret.height = canvas.height
  const retCtx = ret.getContext('2d')
  if (retCtx === null) {
    throw new L.ScamperError('Runtime', 'could not get 2d rendering context')
  }
  retCtx.putImageData(outImg, 0, 0)
  return ret
}

function imageGetPixel (canvas: HTMLCanvasElement, x: number, y: number): L.Struct {
  checkContract([canvas, x, y], contract('image-get-pixel', [imageS, C.integer, C.integer]))
  const ctx = canvas.getContext('2d')
if (ctx === null) {
  throw new L.ScamperError('Runtime', 'could not get 2d rendering context')
}
  const img = ctx.getImageData(x, y, 1, 1)
  const data = img.data
  return rgb(data[0], data[1], data[2], data[3])
}

function imageToPixels (canvas: HTMLCanvasElement): L.Struct[] {
  checkContract([canvas], contract('image-to-pixels', [imageS]))
  const ctx = canvas.getContext('2d')
if (ctx === null) {
  throw new L.ScamperError('Runtime', 'could not get 2d rendering context')
}
  const src = ctx.getImageData(0, 0, canvas.width, canvas.height).data
  const ret: L.Struct[] = []
  for (let i = 0; i < src.length; i += 4) {
    ret.push(rgb(src[i], src[i + 1], src[i + 2], src[i + 3]))
  }
  return ret
}

function pixelsToImage (pixels: L.Struct[], width: number, height: number): HTMLCanvasElement {
  checkContract([pixels, width, height], contract('pixels-to-image', [C.vector, C.integer, C.integer]))
  const ret = document.createElement('canvas')
  ret.width = width
  ret.height = height
  const retCtx = ret.getContext('2d')
  if (retCtx === null) {
    throw new L.ScamperError('Runtime', 'could not get 2d rendering context')
  }
  const outImg = retCtx.createImageData(width, height)
  const data = outImg.data
  for (let i = 0; i < pixels.length; i++) {
    const c = pixels[i]
    if (!L.isStructKind(c, 'rgb')) {
      throw new L.ScamperError('Runtime', `pixels->image expected rgb values, received ${L.typeOf(c)}`)
    }
    const color = c as ReturnType<typeof rgb>
    data[i * 4] = color.red
    data[i * 4 + 1] = color.green
    data[i * 4 + 2] = color.blue
    data[i * 4 + 3] = color.alpha
  }
  retCtx.putImageData(outImg, 0, 0)
  return ret
}

function canvasSetPixels (canvas: HTMLCanvasElement, pixels: L.Struct[]): void {
  checkContract([canvas, pixels], contract('canvas-set-pixels!', [imageS, C.vector]))
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new L.ScamperError('Runtime', 'could not get 2d rendering context')
  }
  const outImg = ctx.createImageData(canvas.width, canvas.height)
  const data = outImg.data
  for (let i = 0; i < pixels.length; i++) {
    const c = pixels[i]
    if (!L.isStructKind(c, 'rgb')) {
      throw new L.ScamperError('Runtime', `canvas-set-pixels! expected rgb values, received ${L.typeOf(c)}`)
    }
    const color = c as ReturnType<typeof rgb>
    data[i * 4] = color.red
    data[i * 4 + 1] = color.green
    data[i * 4 + 2] = color.blue
    data[i * 4 + 3] = color.alpha
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
      const target = e.target
      if (target === null) {
        return
      }
      const result = target.result
      if (typeof result !== 'string') {
        throw new L.ScamperError('Runtime', 'failed to load image as data URL')
      }
    
      const img = new Image()
      img.onload = () => {
        outp.innerHTML = ''
        const canvas = document.createElement('canvas')
        canvas.width = img.width
        canvas.height = img.height
        const ctx = canvas.getContext('2d')
        if (ctx === null) {
          throw new L.ScamperError('Runtime', 'could not get 2d rendering context')
        }
        ctx.drawImage(img, 0, 0)
        try {
          const v = L.callScamperFn(rif.callback, canvas)
          outp.appendChild(HtmlRenderer.render(v))
        } catch (e) {
          outp.appendChild(HtmlRenderer.render(e as L.ScamperError))
        }
      }
      img.src = result
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

HtmlRenderer.registerCustomRenderer(isReactiveImageFile, (v: L.Value) => render(v as ReactiveImageFile))

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