import * as L from '../../lpm'
import { Drawing, drawing_normalize, drawing_render } from '../image/drawing.js'
import { Rgb, color_colorToRgb, color_rgb, color_rgbToString } from '../image/color.js'
import { Font, font_font, font_fontToFontString } from '../image/font.js'
import { context2d } from '../image/context.js'
import { requireBrowser } from '../browser.js'

// N.B., a type predicate, so the guards below narrow rather than cast (#553).
export function canvas_canvasQ(v: L.Value): v is HTMLCanvasElement {
  return typeof HTMLCanvasElement !== 'undefined' && v instanceof HTMLCanvasElement
}

// N.B., canvas-width/canvas-height used to be bound to the *drawing* accessors,
// which accept either kind via a cast. Splitting them gives each an honest
// contract (#103).
export function canvas_canvasWidth(canvas: L.Value): number {
  // The contract is `canvas?`, so what arrives is a canvas and this re-narrows
  // it: `pixel-map` names this at top level, which reaches the native without
  // the contract (#553), and anything with a `width` field answered instead.
  if (!canvas_canvasQ(canvas)) {
    throw new L.ScamperError('Runtime', 'canvas-width: expected a canvas')
  }
  return canvas.width
}

export function canvas_canvasHeight(canvas: L.Value): number {
  // As canvas-width (#553).
  if (!canvas_canvasQ(canvas)) {
    throw new L.ScamperError('Runtime', 'canvas-height: expected a canvas')
  }
  return canvas.height
}

export function canvas_makeCanvas(width: number, height: number): HTMLCanvasElement {
  requireBrowser()
  const canvas = document.createElement('canvas')
  canvas.width = width
  canvas.height = height
  return canvas
}

export function canvas_canvasRectangle(canvas: HTMLCanvasElement, x: number, y: number, width: number, height: number, mode: string, color: L.Value): void {
  const ctx = context2d(canvas)
  ctx.fillStyle = color_rgbToString(color_colorToRgb(color))
  ctx.strokeStyle = color_rgbToString(color_colorToRgb(color))
  if (mode === 'solid') {
    ctx.fillRect(x, y, width, height)
  } else if (mode === 'outline') {
    ctx.strokeRect(x, y, width, height)
  } else {
    throw new L.ScamperError('Runtime', `canvas-rectangle!: expected "solid" or "outline", but got ${mode}`)
  }
}

export function canvas_canvasEllipse(canvas: HTMLCanvasElement, x: number, y: number, radiusX: number, radiusY: number, rotation: number, startAngle: number, endAngle: number, mode: string, color: L.Value): void {
  const ctx = context2d(canvas)
  ctx.fillStyle = color_rgbToString(color_colorToRgb(color))
  ctx.strokeStyle = color_rgbToString(color_colorToRgb(color))
  ctx.beginPath()
  ctx.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle)
  if (mode === 'solid') {
    ctx.fill()
  } else if (mode === 'outline') {
    ctx.stroke()
  } else {
    throw new L.ScamperError('Runtime', `canvas-ellipse!: expected "solid" or "outline", but got ${mode}`)
  }
}

export function canvas_canvasCircle(canvas: HTMLCanvasElement, x: number, y: number, radius: number, mode: string, color: string): void {
  const ctx = context2d(canvas)
  ctx.fillStyle = color_rgbToString(color_colorToRgb(color))
  ctx.strokeStyle = color_rgbToString(color_colorToRgb(color))
  ctx.beginPath()
  ctx.arc(x, y, radius, 0, 2 * Math.PI)
  if (mode === 'solid') {
    ctx.fill()
  } else if (mode === 'outline') {
    ctx.stroke()
  } else {
    throw new L.ScamperError('Runtime', `canvas-circle!: expected "solid" or "outline", but got ${mode}`)
  }
}

export function canvas_canvasText(canvas: HTMLCanvasElement, x: number, y: number, text: string, size: number, mode: string, color: L.Value, font?: Font): void {
  const f: Font = font ?? font_font('Arial')

  const ctx = context2d(canvas)
  ctx.fillStyle = color_rgbToString(color_colorToRgb(color))
  ctx.strokeStyle = color_rgbToString(color_colorToRgb(color))
  ctx.font = font_fontToFontString(f, size)
  if (mode === 'solid') {
    ctx.fillText(text, x, y)
  } else if (mode === 'outline') {
    ctx.strokeText(text, x, y)
  } else {
    throw new L.ScamperError('Runtime', `canvas-text!: expected "solid" or "outline", but got ${mode}`)
  }
}

export function canvas_canvasDrawing(canvas: HTMLCanvasElement, x: number, y: number, drawing: Drawing): void {
  // Normalised so a nested rotation paints where `drawing-width` says it is.
  drawing_render(x, y, drawing_normalize(drawing), canvas)
}

export function canvas_canvasPath(canvas: HTMLCanvasElement, lst: L.List, mode: string, color: L.Value): void {
  const ctx = context2d(canvas)
  const pairs = L.listToVector(lst)
  if (mode !== 'solid' && mode !== 'outline') {
    throw new L.ScamperError('Runtime', `canvas-path!: expected "solid" or "outline", but got ${mode}`)
  }
  if (pairs.length === 0) {
    return
  }

  ctx.fillStyle = color_rgbToString(color_colorToRgb(color))
  ctx.strokeStyle = color_rgbToString(color_colorToRgb(color))
  ctx.beginPath()
  let p: L.Pair = pairs[0] as L.Pair
  ctx.moveTo(p.fst as number, p.snd as number)
  for (let i = 1; i < pairs.length; i++) {
    p = pairs[i] as L.Pair
    ctx.lineTo(p.fst as number, p.snd as number)
  }
  if (mode === 'solid') {
    ctx.fill()
  } else {
    ctx.stroke()
  }
}

export function canvas_animateWith(fn: L.ScamperFn): void {
  requireBrowser()
  // Each frame runs `(fn time)` as a fiber; the *next* frame is only requested
  // once that fiber finishes (in onComplete), which both moves the loop's
  // continuation past the async boundary and gives natural back-pressure -- no
  // pile-up of frame fibers. The loop continues only while the callback returns
  // #t; #f, a non-boolean, or an error (result === null) lets the animation die.
  // The run's AbortSignal also stops the loop when the program is re-run/stopped.
  //
  // The run is captured here, at registration, not looked up inside the frame
  // callback: by the time a frame fires nothing is stepping, so a later lookup
  // would find the foreground program rather than this one -- wrong as soon as
  // a page holds several (#375).
  const run = L.currentRun()

  function callback(time: number) {
    if (run.signal?.aborted) {
      return
    }
    run.spawn(fn, [time], (result) => {
      if (result === true && !run.signal?.aborted) {
        window.requestAnimationFrame(callback)
      }
    })
  }

  window.requestAnimationFrame(callback)
}

export function canvas_canvasOnclick(canvas: HTMLCanvasElement, fn: L.ScamperFn): void {
  // Each click runs `(fn x y)` (the click offset) as a fresh fiber; errors
  // surface in the output pane. The run's AbortSignal removes the listener when
  // the program is re-run/stopped.
  const run = L.currentRun()
  canvas.addEventListener(
    'click',
    (ev: MouseEvent) => { run.spawn(fn, [ev.offsetX, ev.offsetY]) },
    { signal: run.signal },
  )
}

/***** Per-pixel manipulation *************************************************/

// N.B., pixel-map is now defined in image.scm (Scheme) on top of image->pixels,
// vector-map, and pixels->image -- a JS implementation can no longer call the
// per-pixel Scamper function (callScamperFn is disabled).

export function canvas_canvasGetPixel(canvas: HTMLCanvasElement, x: number, y: number): L.Struct {
  const ctx = context2d(canvas)
  const img = ctx.getImageData(x, y, 1, 1)
  const data = img.data
  return color_rgb(data[0], data[1], data[2], data[3])
}

/***** Pixels ****************************************************************/

/**
 * Pixels: a vector of rgb values, what canvas->pixels produces and
 * pixels->canvas consumes. There is no distinct runtime type -- this is the
 * contract the pixel operations were previously declared `any` for.
 */
export function canvas_pixelsQ(v: L.Value): v is Rgb[] {
  return L.isArray(v) && v.every((p) => L.isStructKind(p, 'rgba'))
}

export function canvas_canvasToPixels(canvas: L.Value): L.Struct[] {
  // As canvas-width: `pixel-map` reaches this one without its `canvas?`
  // contract (#553).
  if (!canvas_canvasQ(canvas)) {
    throw new L.ScamperError('Runtime', 'canvas->pixels: expected a canvas')
  }
  const ctx = context2d(canvas)
  const src = ctx.getImageData(0, 0, canvas.width, canvas.height).data
  const ret = []
  for (let i = 0; i < src.length; i += 4) {
    ret.push(color_rgb(src[i], src[i + 1], src[i + 2], src[i + 3]))
  }
  return ret
}

export function canvas_pixelsToCanvas(pixels: L.Value, width: L.Value, height: L.Value): HTMLCanvasElement {
  requireBrowser()
  // The contract is `pixels?`, `integer?`, `integer?`. `pixel-map` names this
  // at top level and so skips it (#553): a non-rgb element read `undefined`
  // for each channel, and assigning undefined into a Uint8ClampedArray stores
  // 0 -- an all-black, fully transparent canvas, with no error at all.
  if (!canvas_pixelsQ(pixels)) {
    throw new L.ScamperError('Runtime', 'pixels->canvas: expected a vector of rgb values')
  }
  if (!L.isNumber(width) || !Number.isInteger(width) ||
      !L.isNumber(height) || !Number.isInteger(height)) {
    throw new L.ScamperError('Runtime', 'pixels->canvas: expected an integer width and height')
  }
  const ret = document.createElement('canvas')
  ret.width = width
  ret.height = height
  const ctx = context2d(ret)
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

export function canvas_canvasSetPixels(canvas: HTMLCanvasElement, pixels: L.Value): void {
  // The same guard pixels->canvas carries, and for the same reason: a non-rgb
  // element reads `undefined` for each channel, and assigning undefined into a
  // Uint8ClampedArray stores 0 -- a silently black, fully transparent canvas
  // rather than an error (#553, via pixel-map). No library definition calls
  // this today, so its contract always runs and the guard is unreachable; it
  // is here so the two do not disagree if one ever is called.
  if (!canvas_pixelsQ(pixels)) {
    throw new L.ScamperError('Runtime', 'canvas-set-pixels!: expected a vector of rgb values')
  }
  const ctx = context2d(canvas)
  const outImg = ctx.createImageData(canvas.width, canvas.height)
  const data = outImg.data
  const px = pixels as L.Struct[]
  for (let i = 0; i < px.length; i++) {
    const c = px[i] as Rgb
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