import * as L from '../lpm'
import { checkContract, contract } from './contract.js'
import * as C from './contract.js'
import { Drawing, render } from './image/drawing.js'
import { colorToRgb, colorS, rgbToString } from './image/color.js'
import { Font, font, fontS, fontToFontString } from './image/font.js'

const Canvas: L.Library = new L.Library()
type CanvasColor = L.Value

function makeCanvas (width: number, height: number): HTMLCanvasElement {
  checkContract([width, height], contract('make-canvas', [C.integer, C.integer]))
  const canvas = document.createElement('canvas')
  canvas.width = width
  canvas.height = height
  return canvas
}
Canvas.registerValue('make-canvas', makeCanvas)

function canvasRectangle (canvas: HTMLCanvasElement, x: number, y: number, width: number, height: number, mode: string, color: CanvasColor): void {
  checkContract([canvas, x, y, width, height, mode, color], contract('canvas-rectangle!', [C.any, C.integer, C.integer, C.integer, C.integer, C.string, colorS]))
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new L.ScamperError('Runtime', 'canvas-rectangle!: could not get 2d context')
  }
  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
  if (mode === 'solid') {
    ctx.fillRect(x, y, width, height)
  } else if (mode === 'outline') {
    ctx.strokeRect(x, y, width, height)
  } else {
    throw new L.ScamperError('Runtime', `canvas-rectangle!: expected "solid" or "outline", but got ${mode}`)
  }
}
Canvas.registerValue('canvas-rectangle!', canvasRectangle)

function canvasEllipse (canvas: HTMLCanvasElement, x: number, y: number, radiusX: number, radiusY: number, rotation: number, startAngle: number, endAngle: number, mode: string, color: CanvasColor): void {
  checkContract([canvas, x, y, radiusX, radiusY, rotation, startAngle, endAngle, mode, color], contract('canvas-ellipse!', [C.any, C.number, C.number, C.number, C.number, C.number, C.number, C.number, C.string, colorS]))
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new L.ScamperError('Runtime', 'canvas-rectangle!: could not get 2d context')
  }
  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
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
Canvas.registerValue('canvas-ellipse!', canvasEllipse)

function canvasCircle (canvas: HTMLCanvasElement, x: number, y: number, radius: number, mode: string, color: string): void {
  checkContract([canvas, x, y, radius, mode, color], contract('canvas-circle!', [C.any, C.number, C.number, C.number, C.string, colorS]))
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new L.ScamperError('Runtime', 'canvas-rectangle!: could not get 2d context')
  }
  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
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
Canvas.registerValue('canvas-circle!', canvasCircle)

function canvasText (canvas: HTMLCanvasElement, x: number, y: number, text: string, size: number, mode: string, color: CanvasColor, ...rest: L.Value[]): void {
  checkContract([canvas, x, y, text, size, mode, color, ...rest], contract('canvas-text!', [C.any, C.integer, C.integer, C.string, C.nonneg, C.string, colorS], C.any))
  let f: Font = font('Arial')
  if (rest.length > 1) {
    throw new L.ScamperError('Runtime', `wrong number of arguments to canvas-text! provided. Expected 7 or 8, received ${String(rest.length + 7)}.`)
  } else if (rest.length == 1) {
    if (fontS.predicate(rest[0])) {
      f = rest[0] as Font
    } else {
      throw new L.ScamperError('Runtime', fontS.errorMsg(rest[0]))
    }
  }

  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new L.ScamperError('Runtime', 'canvas-text!: could not get 2d context')
  }
  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
  ctx.font = fontToFontString(f, size)
  if (mode === 'solid') {
    ctx.fillText(text, x, y)
  } else if (mode === 'outline') {
    ctx.strokeText(text, x, y)
  } else {
    throw new L.ScamperError('Runtime', `canvas-text!: expected "solid" or "outline", but got ${mode}`)
  }
}
Canvas.registerValue('canvas-text!', canvasText)

function canvasDrawing (canvas: HTMLCanvasElement, x: number, y: number, drawing: Drawing): void {
  checkContract([canvas, x, y, drawing], contract('canvas-drawing!', [C.any, C.integer, C.integer, C.any]))
  render(x, y, drawing, canvas)
}
Canvas.registerValue('canvas-drawing!', canvasDrawing)

function canvasPath (canvas: HTMLCanvasElement, lst: L.List, mode: string, color: CanvasColor): void {
  checkContract([canvas, lst, mode, color], contract('canvas-path!', [C.any, C.list, C.string, C.string]))
  const ctx = canvas.getContext('2d')
  if (ctx === null) {
    throw new L.ScamperError('Runtime', 'canvas-path!: could not get 2d context')
  }
  const pairs = L.listToVector(lst)
  if (mode !== 'solid' && mode !== 'outline') {
    throw new L.ScamperError('Runtime', `canvas-path!: expected "solid" or "outline", but got ${mode}`)
  }
  if (pairs.length === 0) {
    return
  }

  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
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
Canvas.registerValue('canvas-path!', canvasPath)

function animateWith (fn: L.ScamperFn): void {
  checkContract([fn], contract('animate-with', [C.func]))
  function callback (time: number) {
    let result: boolean
    try {
      result = L.callScamperFn(fn, time)
    } catch (e) {
      alert(`animate-with callback threw an error:\n\n${(e as Error).toString()}`)
      return
    }
    if (typeof result !== 'boolean') {
      alert(`animate-with callback returned a non-boolean value: ${String(result)}`)
    } else if (result) {
      window.requestAnimationFrame(callback)
    }
    // Otherwise, let the animation die!
  }
  window.requestAnimationFrame(callback)
}
Canvas.registerValue('animate-with', animateWith)

function canvasOnclick (canvas: HTMLCanvasElement, fn: L.ScamperFn): void {
  checkContract([canvas, fn], contract('canvas-onclick!', [C.any, C.func]))
  canvas.onclick = function (ev: MouseEvent) {
    try {
      console.log(`offset: (${String(ev.offsetX)}, ${String(ev.offsetY)}), client: (${String(ev.clientX)}, ${String(ev.clientY)}), page: (${String(ev.pageX)}, ${String(ev.pageY)})`)
      L.callScamperFn(fn, ev.offsetX, ev.offsetY)
    } catch (e) {
      alert(`canvas-onclick! callback threw an error:\n\n${(e as Error).toString()}`)
      return
    }
  }
}

Canvas.registerValue('canvas-onclick!', canvasOnclick)

export default Canvas