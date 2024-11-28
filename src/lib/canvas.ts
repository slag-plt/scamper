import { ScamperError, Value } from '../lang.js'
import * as L from '../lang.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { Drawing, render } from './image/drawing.js'
import { callFunction } from '../sem.js'
import { colorToRgb, colorS, rgbToString } from './image/color.js'
import { Font, font, fontS, fontToFontString } from './image/font.js'

const Canvas: L.Library = L.emptyLibrary()

function makeCanvas (width: number, height: number): HTMLCanvasElement {
  checkContract(arguments, contract('make-canvas', [C.integer, C.integer]))
  const canvas = document.createElement('canvas')
  canvas.width = width
  canvas.height = height
  return canvas
}
L.registerValue('make-canvas', makeCanvas, Canvas)

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
L.registerValue('canvas-rectangle!', canvasRectangle, Canvas)

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
L.registerValue('canvas-ellipse!', canvasEllipse, Canvas)

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
L.registerValue('canvas-circle!', canvasCircle, Canvas)

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
L.registerValue('canvas-text!', canvasText, Canvas)

function canvasDrawing (canvas: HTMLCanvasElement, x: number, y: number, drawing: Drawing): void {
  checkContract(arguments, contract('canvas-drawing!', [C.any, C.integer, C.integer, C.any]))
  render(x, y, drawing, canvas)
}
L.registerValue('canvas-drawing!', canvasDrawing, Canvas)

function canvasPath (canvas: HTMLCanvasElement, lst: Value.List, mode: string, color: any): void {
  checkContract(arguments, contract('canvas-path!', [C.any, C.list, C.string, C.string]))
  const ctx: CanvasRenderingContext2D = canvas.getContext('2d')!
  const pairs = L.Value.listToVector(lst)
  if (mode !== 'solid' && mode !== 'outline') {
    throw new ScamperError('Runtime', `canvas-path!: expected "solid" or "outline", but got ${mode}`)
  }
  if (pairs.length === 0) {
    return
  }

  ctx.fillStyle = rgbToString(colorToRgb(color))
  ctx.strokeStyle = rgbToString(colorToRgb(color))
  ctx.beginPath()
  let p: L.Value.Pair = pairs[0] as L.Value.Pair
  ctx.moveTo(p.fst as number, p.snd as number)
  for (let i = 1; i < pairs.length; i++) {
    p = pairs[i] as L.Value.Pair
    ctx.lineTo(p.fst as number, p.snd as number)
  }
  if (mode === 'solid') {
    ctx.fill()
  } else {
    ctx.stroke()
  }
}
L.registerValue('canvas-path!', canvasPath, Canvas)

function animateWith (fn: L.Value.ScamperFn): void {
  checkContract(arguments, contract('animate-with', [C.func]))
  function callback (time: number) {
    let result = false
    try {
      result = callFunction(fn, time)
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
L.registerValue('animate-with', animateWith, Canvas)

function canvasOnclick (canvas: HTMLCanvasElement, fn: L.Value.ScamperFn): void {
  checkContract(arguments, contract('canvas-onclick!', [C.any, C.func]))
  canvas.onclick = function (ev: MouseEvent) {
    try {
      console.log(`offset: (${ev.offsetX}, ${ev.offsetY}), client: (${ev.clientX}, ${ev.clientY}), page: (${ev.pageX}, ${ev.pageY})`)
      callFunction(fn, ev.offsetX, ev.offsetY)
    } catch (e) {
      alert(`canvas-onclick! callback threw an error:\n\n${(e as Error).toString()}`)
      return
    }
  }
}

L.registerValue('canvas-onclick!', canvasOnclick, Canvas)

export default Canvas