import * as L from '../../lpm'
import { Drawing, image_render } from '../image/drawing.js'
import { image_colorToRgb, image_rgbToString } from '../image/color.js'
import { Font, image_font, image_fontQ, image_fontToFontString } from '../image/font.js'

export function canvas_canvasQ(v: any): boolean {
  return v instanceof HTMLCanvasElement
}

export function canvas_makeCanvas(width: number, height: number): HTMLCanvasElement {
  const canvas = document.createElement('canvas')
  canvas.width = width
  canvas.height = height
  return canvas
}

export function canvas_canvasRectangle(canvas: HTMLCanvasElement, x: number, y: number, width: number, height: number, mode: string, color: any): void {
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = image_rgbToString(image_colorToRgb(color))
  ctx.strokeStyle = image_rgbToString(image_colorToRgb(color))
  if (mode === 'solid') {
    ctx.fillRect(x, y, width, height)
  } else if (mode === 'outline') {
    ctx.strokeRect(x, y, width, height)
  } else {
    throw new L.ScamperError('Runtime', `canvas-rectangle!: expected "solid" or "outline", but got ${mode}`)
  }
}

export function canvas_canvasEllipse(canvas: HTMLCanvasElement, x: number, y: number, radiusX: number, radiusY: number, rotation: number, startAngle: number, endAngle: number, mode: string, color: any): void {
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = image_rgbToString(image_colorToRgb(color))
  ctx.strokeStyle = image_rgbToString(image_colorToRgb(color))
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
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = image_rgbToString(image_colorToRgb(color))
  ctx.strokeStyle = image_rgbToString(image_colorToRgb(color))
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

export function canvas_canvasText(canvas: HTMLCanvasElement, x: number, y: number, text: string, size: number, mode: string, color: any, ...rest: any[]): void {
  let f: Font = image_font('Arial')
  if (rest.length > 1) {
    throw new L.ScamperError('Runtime', `wrong number of arguments to canvas-text! provided. Expected 7 or 8, received ${arguments.length}.`)
  } else if (rest.length == 1) {
    if (image_fontQ(rest[0])) {
      f = rest[0] as Font
    } else {
      throw new L.ScamperError('Runtime', `expected a font, received ${L.typeOf(rest[0])}`)
    }
  }

  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = image_rgbToString(image_colorToRgb(color))
  ctx.strokeStyle = image_rgbToString(image_colorToRgb(color))
  ctx.font = image_fontToFontString(f, size)
  if (mode === 'solid') {
    ctx.fillText(text, x, y)
  } else if (mode === 'outline') {
    ctx.strokeText(text, x, y)
  } else {
    throw new L.ScamperError('Runtime', `canvas-text!: expected "solid" or "outline", but got ${mode}`)
  }
}

export function canvas_canvasDrawing(canvas: HTMLCanvasElement, x: number, y: number, drawing: Drawing): void {
  image_render(x, y, drawing, canvas)
}

export function canvas_canvasPath(canvas: HTMLCanvasElement, lst: L.List, mode: string, color: any): void {
  const ctx: CanvasRenderingContext2D = canvas.getContext('2d')!
  const pairs = L.listToVector(lst)
  if (mode !== 'solid' && mode !== 'outline') {
    throw new L.ScamperError('Runtime', `canvas-path!: expected "solid" or "outline", but got ${mode}`)
  }
  if (pairs.length === 0) {
    return
  }

  ctx.fillStyle = image_rgbToString(image_colorToRgb(color))
  ctx.strokeStyle = image_rgbToString(image_colorToRgb(color))
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
  function callback (time: number) {
    let result = false
    try {
      result = L.callScamperFn(fn, time) as boolean
    } catch (e) {
      alert(`animate-with callback threw an error:\n\n${(e as Error).toString()}`)
      return
    }
    if (typeof result !== 'boolean') {
      alert(`animate-with callback returned a non-boolean value: ${result}`)
    } else if (result) {
      window.requestAnimationFrame(callback)
    }
    // Otherwise, let the animation die!
  }
  window.requestAnimationFrame(callback)
}

export function canvas_canvasOnclick(canvas: HTMLCanvasElement, fn: L.ScamperFn): void {
  canvas.onclick = function (ev: MouseEvent) {
    try {
      console.log(`offset: (${ev.offsetX}, ${ev.offsetY}), client: (${ev.clientX}, ${ev.clientY}), page: (${ev.pageX}, ${ev.pageY})`)
      L.callScamperFn(fn, ev.offsetX, ev.offsetY)
    } catch (e) {
      alert(`canvas-onclick! callback threw an error:\n\n${(e as Error).toString()}`)
      return
    }
  }
}

