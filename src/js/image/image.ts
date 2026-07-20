import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import HtmlRenderer from '../../lpm/renderers/html.js'
import * as L from '../../lpm'
import { Rgb, image_rgb } from './color.js'

const imageS: C.Spec = {
  predicate: (v: any) => v instanceof HTMLCanvasElement,
  errorMsg: (actual: any) => `expected an image, received ${L.typeOf(actual)}`
}

/***** Image loading **********************************************************/

export interface ReactiveImageFile extends L.Struct {
  [L.structKind]: 'reactive-image-file',
  callback: L.ScamperFn
}

export function image_withImageFile(callback: L.ScamperFn): ReactiveImageFile {
  checkContract(arguments, contract('with-image-file', [C.func]))  
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-image-file',
    callback
  }
}

export function image_isReactiveImageFile (v: any): boolean {
  return L.isStructKind(v, 'reactive-image-file')
}

export function image_withImageFromUrl(url: string, callback: L.ScamperFn): HTMLElement {
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

export function image_pixelMap(fn: L.ScamperFn, canvas: HTMLCanvasElement): HTMLCanvasElement {
  checkContract(arguments, contract('pixel-map', [C.func, imageS]))
  const ctx = canvas.getContext('2d')!
  const inpImg = ctx.getImageData(0, 0, canvas.width, canvas.height)
  const src = inpImg.data

  const outImg = ctx.createImageData(canvas.width, canvas.height)
  const dst = outImg.data
  for (let i = 0; i < src.length; i += 4) {
    const c = L.callScamperFn(fn, image_rgb(src[i], src[i + 1], src[i+2], src[i+3])) as Rgb
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

export function image_imageGetPixel(canvas: HTMLCanvasElement, x: number, y: number): L.Struct {
  checkContract(arguments, contract('image-get-pixel', [imageS, C.integer, C.integer]))
  const ctx = canvas.getContext('2d')!
  const img = ctx.getImageData(x, y, 1, 1)
  const data = img.data
  return image_rgb(data[0], data[1], data[2], data[3])
}

export function image_imageToPixels(canvas: HTMLCanvasElement): L.Struct[] {
  checkContract(arguments, contract('image-to-pixels', [imageS]))
  const ctx = canvas.getContext('2d')!
  const src = ctx.getImageData(0, 0, canvas.width, canvas.height).data
  const ret = []
  for (let i = 0; i < src.length; i += 4) {
    ret.push(image_rgb(src[i], src[i + 1], src[i + 2], src[i + 3]))
  }
  return ret
}

export function image_pixelsToImage(pixels: L.Struct[], width: number, height: number): HTMLCanvasElement {
  checkContract(arguments, contract('pixels-to-image', [C.vector, C.integer, C.integer]))
  const ret = document.createElement('canvas')
  ret.width = width
  ret.height = height
  const ctx = ret.getContext('2d')!
  const outImg = ctx.createImageData(width, height)
  const data = outImg.data
  for (let i = 0; i < pixels.length; i++) {
    const c = pixels[i] as Rgb
    data[i*4] = c.red
    data[i*4 + 1] = c.green
    data[i*4 + 2] = c.blue
    data[i*4 + 3] = c.alpha
  }
  ctx.putImageData(outImg, 0, 0)
  return ret
}

export function image_canvasSetPixels(canvas: HTMLCanvasElement, pixels: L.Struct[]): void {
  checkContract(arguments, contract('canvas-set-pixels!', [imageS, C.vector]))
  const ctx = canvas.getContext('2d')!
  const outImg = ctx.createImageData(canvas.width, canvas.height)
  const data = outImg.data
  for (let i = 0; i < pixels.length; i++) {
    const c = pixels[i] as Rgb
    data[i*4] = c.red
    data[i*4 + 1] = c.green
    data[i*4 + 2] = c.blue
    data[i*4 + 3] = c.alpha
  }
  ctx.putImageData(outImg, 0, 0)
}

/***** Exports ****************************************************************/

// Image loading

// Per-pixel manipulation