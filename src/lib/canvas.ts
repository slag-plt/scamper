import { ScamperError } from '../lang.js'
import * as L from '../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { Drawing, render } from './image/drawing.js'
import { colorToRgb, colorS, rgbToString } from './image/color.js'
import { Font, font, fontS, fontToFontString } from './image/font.js'

const Canvas: L.Library = new L.Library()

function makeCanvas (width: number, height: number): HTMLCanvasElement {
  checkContract(arguments, contract('make-canvas', [C.integer, C.integer]))
  const canvas = document.createElement('canvas')
  canvas.width = width
  canvas.height = height
  return canvas
}
Canvas.registerValue('make-canvas', makeCanvas)

function canvasRectangle (canvas: HTMLCanvasElement, x: number, y: number, width: number, height: number, mode: string, color: any): void {
  checkContract(arguments, contract('canvas-rectangle!', [C.any, C.integer, C.integer, C.integer, C.integer, C.string, colorS]))
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
  if (mode === 'solid') {
    ctx.fillRect(x, y, width, height)
  } else if (mode === 'outline') {
    ctx.strokeRect(x, y, width, height)
  } else {
    throw new ScamperError('Runtime', `canvas-rectangle!: expected "solid" or "outline", but got ${mode}`)
  }
}
Canvas.registerValue('canvas-rectangle!', canvasRectangle)

function canvasEllipse (canvas: HTMLCanvasElement, x: number, y: number, radiusX: number, radiusY: number, rotation: number, startAngle: number, endAngle: number, mode: string, color: any): void {
  checkContract(arguments, contract('canvas-ellipse!', [C.any, C.number, C.number, C.number, C.number, C.number, C.number, C.number, C.string, colorS]))
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
  ctx.beginPath()
  ctx.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle)
  if (mode === 'solid') {
    ctx.fill()
  } else if (mode === 'outline') {
    ctx.stroke()
  } else {
    throw new ScamperError('Runtime', `canvas-ellipse!: expected "solid" or "outline", but got ${mode}`)
  }
}
Canvas.registerValue('canvas-ellipse!', canvasEllipse)

function canvasCircle (canvas: HTMLCanvasElement, x: number, y: number, radius: number, mode: string, color: string): void {
  checkContract(arguments, contract('canvas-circle!', [C.any, C.number, C.number, C.number, C.string, colorS]))
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
  ctx.beginPath()
  ctx.arc(x, y, radius, 0, 2 * Math.PI)
  if (mode === 'solid') {
    ctx.fill()
  } else if (mode === 'outline') {
    ctx.stroke()
  } else {
    throw new ScamperError('Runtime', `canvas-circle!: expected "solid" or "outline", but got ${mode}`)
  }
}
Canvas.registerValue('canvas-circle!', canvasCircle)

function canvasText (canvas: HTMLCanvasElement, x: number, y: number, text: string, size: number, mode: string, color: any, ...rest: any[]): void {
  checkContract(arguments, contract('canvas-text!', [C.any, C.integer, C.integer, C.string, C.nonneg, C.string, colorS], C.any))
  let f: Font = font('Arial')
  if (rest.length > 1) {
    throw new ScamperError('Runtime', `wrong number of arguments to canvas-text! provided. Expected 7 or 8, received ${arguments.length}.`)
  } else if (rest.length == 1) {
    if (fontS.predicate(rest[0])) {
      f = rest[0] as Font
    } else {
      throw new ScamperError('Runtime', fontS.errorMsg(rest[0]))
    }
  }

  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
  ctx.font = fontToFontString(f, size)
  if (mode === 'solid') {
    ctx.fillText(text, x, y)
  } else if (mode === 'outline') {
    ctx.strokeText(text, x, y)
  } else {
    throw new ScamperError('Runtime', `canvas-text!: expected "solid" or "outline", but got ${mode}`)
  }
}
Canvas.registerValue('canvas-text!', canvasText)

function canvasDrawing (canvas: HTMLCanvasElement, x: number, y: number, drawing: Drawing): void {
  checkContract(arguments, contract('canvas-drawing!', [C.any, C.integer, C.integer, C.any]))
  render(x, y, drawing, canvas)
}
Canvas.registerValue('canvas-drawing!', canvasDrawing)

function canvasPath (canvas: HTMLCanvasElement, lst: L.List, mode: string, color: any): void {
  checkContract(arguments, contract('canvas-path!', [C.any, C.list, C.string, C.string]))
  const ctx: CanvasRenderingContext2D = canvas.getContext('2d')!
  const pairs = L.listToVector(lst)
  if (mode !== 'solid' && mode !== 'outline') {
    throw new ScamperError('Runtime', `canvas-path!: expected "solid" or "outline", but got ${mode}`)
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
  checkContract(arguments, contract('animate-with', [C.func]))
  function callback (time: number) {
    let result = false
    try {
      result = L.callScamperFn(fn, time)
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
Canvas.registerValue('animate-with', animateWith)

function canvasOnclick (canvas: HTMLCanvasElement, fn: L.ScamperFn): void {
  checkContract(arguments, contract('canvas-onclick!', [C.any, C.func]))
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

Canvas.registerValue('canvas-onclick!', canvasOnclick)

export default Canvas