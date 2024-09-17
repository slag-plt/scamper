import { ScamperError, Value } from '../lang.js'
import * as L from '../lang.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { Drawing, render } from './image/drawing.js'
import { callFunction } from '../sem.js'

const Canvas: L.Library = L.emptyLibrary()

function makeCanvas (width: number, height: number): HTMLCanvasElement {
  checkContract(arguments, contract('make-canvas', [C.integer, C.integer]))
  const canvas = document.createElement('canvas')
  canvas.width = width
  canvas.height = height
  return canvas
}
L.registerValue('make-canvas', makeCanvas, Canvas)

function drawRectangle (canvas: HTMLCanvasElement, x: number, y: number, width: number, height: number, mode: string, color: string): void {
  checkContract(arguments, contract('draw-rectangle', [C.any, C.integer, C.integer, C.integer, C.integer, C.string, C.string]))
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = color
  ctx.strokeStyle = color
  if (mode === 'solid') {
    ctx.fillRect(x, y, width, height)
  } else if (mode === 'outline') {
    ctx.strokeRect(x, y, width, height)
  } else {
    throw new ScamperError('Runtime', `draw-rectangle: expected "solid" or "outline", but got ${mode}`)
  }
}
L.registerValue('draw-rectangle', drawRectangle, Canvas)

function drawEllipse (canvas: HTMLCanvasElement, x: number, y: number, radiusX: number, radiusY: number, rotation: number, startAngle: number, endAngle: number, mode: string, color: string): void {
  checkContract(arguments, contract('draw-ellipse', [C.any, C.number, C.number, C.number, C.number, C.number, C.number, C.number, C.string, C.string]))
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = color
  ctx.strokeStyle = color
  ctx.beginPath()
  ctx.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle)
  if (mode === 'solid') {
    ctx.fill()
  } else if (mode === 'outline') {
    ctx.stroke()
  } else {
    throw new ScamperError('Runtime', `draw-ellipse: expected "solid" or "outline", but got ${mode}`)
  }
}
L.registerValue('draw-ellipse', drawEllipse, Canvas)

function drawCircle (canvas: HTMLCanvasElement, x: number, y: number, radius: number, mode: string, color: string): void {
  checkContract(arguments, contract('draw-circle', [C.any, C.number, C.number, C.number, C.string, C.string]))
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = color
  ctx.strokeStyle = color
  ctx.beginPath()
  ctx.arc(x, y, radius, 0, 2 * Math.PI)
  if (mode === 'solid') {
    ctx.fill()
  } else if (mode === 'outline') {
    ctx.stroke()
  } else {
    throw new ScamperError('Runtime', `draw-circle: expected "solid" or "outline", but got ${mode}`)
  }
}
L.registerValue('draw-circle', drawCircle, Canvas)

function drawText (canvas: HTMLCanvasElement, text: string, x: number, y: number, mode: string, color: string, font: string): void {
  checkContract(arguments, contract('draw-text', [C.any, C.string, C.integer, C.integer, C.string, C.string, C.string]))
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = color
  ctx.strokeStyle = color
  ctx.font = font
  if (mode === 'solid') {
    ctx.fillText(text, x, y)
  } else if (mode === 'outline') {
    ctx.strokeText(text, x, y)
  } else {
    throw new ScamperError('Runtime', `draw-text: expected "solid" or "outline", but got ${mode}`)
  }
}
L.registerValue('draw-text', drawText, Canvas)

function drawDrawing (canvas: HTMLCanvasElement, drawing: Drawing, x: number, y: number): void {
  checkContract(arguments, contract('draw-drawing', [C.any, C.any, C.integer, C.integer]))
  const ctx = canvas.getContext('2d')!
  render(x, y, drawing, canvas)
}
L.registerValue('draw-drawing', drawDrawing, Canvas)

function drawPath (canvas: HTMLCanvasElement, lst: Value.List, mode: string, color: string): void {
  checkContract(arguments, contract('draw-path', [C.any, C.list, C.string, C.string]))
  const ctx: CanvasRenderingContext2D = canvas.getContext('2d')!
  const pairs = L.Value.listToVector(lst)
  if (mode !== 'solid' && mode !== 'outline') {
    throw new ScamperError('Runtime', `draw-path: expected "solid" or "outline", but got ${mode}`)
  }
  if (pairs.length === 0) {
    return
  }

  ctx.fillStyle = color
  ctx.strokeStyle = color
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
L.registerValue('draw-path', drawPath, Canvas)

function animateWith (fn: L.Value.ScamperFn): void {
  checkContract(arguments, contract('animate-with', [C.func]))
  function callback (time: number) {
    try {
      const result = callFunction(fn, time)
    } catch (e) {
      alert(`animate-with callback threw an error:\n\n${(e as Error).toString()}`)
      return
    }
    window.requestAnimationFrame(callback)
  }
  window.requestAnimationFrame(callback)
}
L.registerValue('animate-with', animateWith, Canvas)

function canvasOnclick (canvas: HTMLCanvasElement, fn: L.Value.ScamperFn): void {
  checkContract(arguments, contract('canvas-onclick', [C.any, C.func]))
  console.log('canvasOnclick')
  canvas.onclick = function (ev: MouseEvent) {
    try {
      console.log(`offset: (${ev.offsetX}, ${ev.offsetY}), client: (${ev.clientX}, ${ev.clientY}), page: (${ev.pageX}, ${ev.pageY})`)
      callFunction(fn, ev.offsetX, ev.offsetY)
    } catch (e) {
      alert(`canvas-onclick callback threw an error:\n\n${(e as Error).toString()}`)
      return
    }
  }
}

L.registerValue('canvas-onclick', canvasOnclick, Canvas)

export default Canvas