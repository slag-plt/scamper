import HtmlRenderer from '../../lpm/renderers/html.js'
import * as L from '../../lpm'
import { Rgb, image_rgb } from './color.js'

/***** Image loading **********************************************************/

export interface ReactiveImageFile extends L.Struct {
  [L.structKind]: 'reactive-image-file',
  callback: L.ScamperFn
}

export function image_withImageFile(callback: L.ScamperFn): ReactiveImageFile {
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

// N.B., pixel-map is now defined in image.scm (Scheme) on top of image->pixels,
// vector-map, and pixels->image -- a JS implementation can no longer call the
// per-pixel Scamper function (callScamperFn is disabled).

export function image_imageGetPixel(canvas: HTMLCanvasElement, x: number, y: number): L.Struct {
  const ctx = canvas.getContext('2d')!
  const img = ctx.getImageData(x, y, 1, 1)
  const data = img.data
  return image_rgb(data[0], data[1], data[2], data[3])
}

export function image_imageToPixels(canvas: HTMLCanvasElement): L.Struct[] {
  const ctx = canvas.getContext('2d')!
  const src = ctx.getImageData(0, 0, canvas.width, canvas.height).data
  const ret = []
  for (let i = 0; i < src.length; i += 4) {
    ret.push(image_rgb(src[i], src[i + 1], src[i + 2], src[i + 3]))
  }
  return ret
}

export function image_pixelsToImage(pixels: L.Struct[], width: number, height: number): HTMLCanvasElement {
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