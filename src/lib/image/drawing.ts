import { checkContract, contract } from '../../contract.js'
import * as C from '../../contract.js'
import * as Render from '../../display.js'
import { Value } from '../../lang.js'

export const lib: [string, Value.T][] = []

function registerFn (name: string, fn: Function): void {
  Value.nameFn(name, fn)
  lib.push([name, fn])
}

function color (r: number, g: number, b: number, a: number): string {
  checkContract(arguments, contract('color', [C.nonneg, C.nonneg, C.nonneg, C.nonneg]))
  return `rgba(${r}, ${g}, ${b}, ${a})`
}
registerFn('color', color)

const modeS: C.Spec = {
  predicate: (v: any) => v === 'solid' || v === 'outline',
  errorMsg: (actual: any) => `expected a mode ("solid" or "outline"), received ${Value.typeOf(actual)}`
}

const alignVS: C.Spec = {
  predicate: (v: any) => v === 'top' || v === 'center' || v === 'bottom',
  errorMsg: (actual: any) => `expected a vertical alignment ("top", "center", or "bottom"), received ${Value.typeOf(actual)}`
}

const alignHS: C.Spec = {
  predicate: (v: any) => v === 'left' || v === 'middle' || v === 'right',
  errorMsg: (actual: any) => `expected a horizontal alignment ("left", "middle", or "right"), received ${Value.typeOf(actual)}`
}

const colorS: C.Spec = {
  // https://stackoverflow.com/questions/48484767/javascript-check-if-string-is-valid-css-color
  predicate: (v: any) => {
    if (typeof v !== 'string') { return false }
    // TODO: need to test this more thoroughly, esp. with rgba strings
    /*
    var s = new Option().style
    s.color = v
    // return 'false' if color wasn't assigned
    return s.color === v.toLowerCase()
    */
   return true
  },
  errorMsg: (actual: any) => `expected a color, received ${Value.typeOf(actual)}`
}

type Mode = 'solid' | 'outline'
export type Drawing = Ellipse | Rectangle | Triangle | Path | Beside | Above | Overlay | OverlayOffset | Rotate | WithDash

function imageQ (v: any): boolean {
  checkContract(arguments, contract('image?', [C.any]))
  return Value.isStructKind(v, 'ellipse') || Value.isStructKind(v, 'rectangle') ||
         Value.isStructKind(v, 'triangle') || Value.isStructKind(v, 'path') ||
         Value.isStructKind(v, 'beside') || Value.isStructKind(v, 'above') ||
         Value.isStructKind(v, 'overlay') || Value.isStructKind(v, 'overlayOffset') ||
         Value.isStructKind(v, 'rotate') || Value.isStructKind(v, 'withDash')
}
registerFn('image?', imageQ)

const imageS = {
  predicate: imageQ,
  errorMsg: (actual: any) => `expected an image, received ${Value.typeOf(actual)}`
}

interface Ellipse extends Value.Struct {
  [Value.structKind]: 'ellipse',
  width: number,
  height: number,
  mode: Mode,
  color: string
}

const ellipsePrim = (width: number, height: number, mode: Mode, color: string): Ellipse => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'ellipse', width, height, mode, color
})

function ellipse (width: number, height: number, mode: Mode, color: string): Ellipse {
  checkContract(arguments, contract('ellipse', [C.nonneg, C.nonneg, modeS, colorS]))
  return ellipsePrim(width, height, mode, color)
}
registerFn('ellipse', ellipse)

function circle (radius: number, mode: Mode, color: string): Ellipse {
  checkContract(arguments, contract('circle', [C.nonneg, modeS, colorS]))
  return ellipsePrim(radius * 2, radius * 2, mode, color)
}
registerFn('circle', circle)

interface Rectangle extends Value.Struct {
  [Value.structKind]: 'rectangle',
  width: number,
  height: number,
  mode: Mode,
  color: string
}

const rectanglePrim = (width: number, height: number, mode: Mode, color: string): Rectangle => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'rectangle', width, height, mode, color
})

function rectangle (width: number, height: number, mode: Mode, color: string): Rectangle {
  checkContract(arguments, contract('rectangle', [C.nonneg, C.nonneg, modeS, colorS]))
  return rectanglePrim(width, height, mode, color)
}
registerFn('rectangle', rectangle)

function square (length: number, mode: Mode, color: string): Rectangle {
  checkContract(arguments, contract('square', [C.nonneg, modeS, colorS]))
  return rectanglePrim(length, length, mode, color)
}
registerFn('square', square)

interface Triangle extends Value.Struct {
  [Value.structKind]: 'triangle',
  width: number,
  height: number,
  length: number,
  mode: Mode,
  color: string
}

const trianglePrim = (length: number, mode: Mode, color: string): Triangle => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'triangle', width: length, height: length * Math.sqrt(3) / 2,
  length, mode, color
})

function triangle (length: number, mode: Mode, color: string): Triangle {
  checkContract(arguments, contract('triangle', [C.nonneg, modeS, colorS]))
  return trianglePrim(length, mode, color)
}
registerFn('triangle', triangle)

interface Path extends Value.Struct {
  [Value.structKind]: 'path',
  width: number,
  height: number,
  points: [number, number][],
  mode: Mode,
  color: string
}

const pathPrim = (width: number, height: number, points: [number, number][], mode: Mode, color: string): Path => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'path', width, height, points, mode, color
})

function path (width: number, height: number, points: Value.List, mode: Mode, color: string): Path {
  checkContract(arguments, contract('path', [C.nonneg, C.nonneg, C.list, modeS, colorS]))
  return pathPrim(width, height, 
    Value.listToVector(points).map((p: Value.T) => [(p as Value.Pair).fst, (p as Value.Pair).snd]) as [number, number][],
    mode, color)
}
registerFn('path', path)

interface Beside extends Value.Struct {
  [Value.structKind]: 'beside',
  align: string,
  width: number,
  height: number,
  drawings: Drawing[]
}

const besideAlignPrim = (align: string, ...drawings: Drawing[]): Beside => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'beside',
  align,
  width: drawings.reduce((acc, d) => acc + d.width, 0),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

function beside (...drawings: Drawing[]): Beside {
  checkContract(arguments, contract('beside', [], imageS)) 
  return besideAlignPrim('center', ...drawings)
}
registerFn('beside', beside)

function besideAlign (align: string, ...drawings: Drawing[]): Beside {
  checkContract(arguments, contract('beside/align', [alignVS], imageS))
  return besideAlignPrim(align, ...drawings)
}
registerFn('beside/align', besideAlign)

interface Above extends Value.Struct {
  [Value.structKind]: 'above',
  align: string,
  width: number,
  height: number,
  drawings: Drawing[]
}

const aboveAlignPrim = (align: string, ...drawings: Drawing[]): Above => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'above',
  align,
  width: Math.max(...drawings.map(d => d.width)),
  height: drawings.reduce((acc, d) => acc + d.height, 0),
  drawings
})

function above (...drawings: Drawing[]): Above {
  checkContract(arguments, contract('above', [], imageS))
  return aboveAlignPrim('middle', ...drawings)
}
registerFn('above', above)

function aboveAlign (align: string, ...drawings: Drawing[]): Above {
  checkContract(arguments, contract('above/align', [alignHS], imageS))
  return aboveAlignPrim(align, ...drawings)
}
registerFn('above/align', aboveAlign)

interface Overlay extends Value.Struct {
  [Value.structKind]: 'overlay',
  xAlign: string,
  yAlign: string,
  width: number,
  height: number,
  drawings: Drawing[]
}

const overlayAlignPrim = (xAlign: string, yAlign: string, ...drawings: Drawing[]): Overlay => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'overlay',
  xAlign,
  yAlign,
  width: Math.max(...drawings.map(d => d.width)),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

function overlay (...drawings: Drawing[]) {
  checkContract(arguments, contract('overlay', [], imageS))
  return overlayAlignPrim('middle', 'center', ...drawings)
}
registerFn('overlay', overlay)

function overlayAlign (xAlign: string, yAlign: string, ...drawings: Drawing[]): Overlay {
  checkContract(arguments, contract('overlay/align', [alignHS, alignVS], imageS))
  return overlayAlignPrim(xAlign, yAlign, ...drawings)
}
registerFn('overlay/align', overlayAlign)

interface OverlayOffset extends Value.Struct {
  [Value.structKind]: 'overlayOffset',
  dx: number,
  dy: number,
  width: number,
  height: number,
  d1: Drawing,
  d2: Drawing
}

function overlayOffset (dx: number, dy: number, d1: Drawing, d2: Drawing): OverlayOffset {
  checkContract(arguments, contract('overlay/offset', [C.number, C.number, imageS, imageS]))
  // N.B., tricky! Need to account for whether (a) we are shifting the smaller
  // or larger image and (b) whether we are shifting it positively or
  // negatively.
  let width
  if (d1.width >= d2.width) {
    width = dx >= 0
      ? Math.max(d1.width, d2.width + Math.abs(dx))
      : Math.abs(dx) + d1.width
  } else {
    width = dx <= 0
      ? Math.max(d2.width, d1.width + Math.abs(dx))
      : Math.abs(dx) + d2.width
  }
  let height
  if (d1.height >= d2.height) {
    height = dy >= 0
      ? Math.max(d1.height, d2.height + Math.abs(dy))
      : Math.abs(dy) + d1.height
  } else {
    height = dy <= 0
      ? Math.max(d2.height, d1.height + Math.abs(dy))
      : Math.abs(dy) + d2.height
  }
  return {
    [Value.scamperTag]: 'struct', [Value.structKind]: 'overlayOffset',
    dx,
    dy,
    // BUG: what if d2 is actually bigger than d1? Then the calculation needs to mirror!
    width,
    height,
    d1,
    d2
  }
}
registerFn('overlay/offset', overlayOffset)

interface Rotate extends Value.Struct {
  [Value.structKind]: 'rotate',
  width: number,
  height: number,
  angle: number,
  drawing: Drawing
}

// const rotate = (angle: number, drawing: Drawing): Rotate => ({
//   tag: 'rotate',
//   width: drawing.width * Math.abs(Math.cos(angle * Math.PI / 180)) + drawing.height * Math.abs(Math.sin(angle * Math.PI / 180)),
//   height: drawing.width * Math.abs(Math.sin(angle * Math.PI / 180)) + drawing.height * Math.abs(Math.cos(angle * Math.PI /180)),
//   angle,
//   drawing
// })

function calculateRotatedBox (width: number, height: number, degrees: number): { width: number, height: number } {
  // Calculate the rotated corners of the box
  const angle = degrees * Math.PI / 180
  const origPoints = [
    [-width / 2, -height / 2],
    [width / 2, -height / 2],
    [-width / 2, height / 2],
    [width / 2, height / 2]
  ]
  const rotatedPoints = origPoints.map(
    ([x, y]) => [
      x * Math.cos(angle) - y * Math.sin(angle),
      x * Math.sin(angle) + y * Math.cos(angle)
    ]
  )

  // Determine the width and height of the box's bounding
  // box by taking mins and maxes of the points.
  const xMin = Math.min(...rotatedPoints.map(([x, _]) => x))
  const xMax = Math.max(...rotatedPoints.map(([x, _]) => x))
  const yMin = Math.min(...rotatedPoints.map(([_, y]) => y))
  const yMax = Math.max(...rotatedPoints.map(([_, y]) => y))

  return {
    width: xMax - xMin,
    height: yMax - yMin
  }
}

function rotate (angle: number, drawing: Drawing): Rotate {
  checkContract(arguments, contract('rotate', [C.number, imageS]))
  const dims = calculateRotatedBox(drawing.width, drawing.height, angle)
  return {
    [Value.scamperTag]: 'struct', [Value.structKind]: 'rotate',
    width: dims.width,
    height: dims.height,
    angle,
    drawing
  }
}
registerFn('rotate', rotate)

interface WithDash extends Value.Struct {
  [Value.structKind]: 'withDash',
  dashSpec: number[],
  drawing: Drawing,
  width: number,
  height: number
}

function withDash (dashSpec: number[], drawing: Drawing): WithDash {
  checkContract(arguments, contract('with-dash', [C.list, imageS])) 
  return {
    [Value.scamperTag]: 'struct', [Value.structKind]: 'withDash',
    dashSpec,
    drawing,
    width: drawing.width,
    height: drawing.height
  }
}
registerFn('with-dash', withDash)

export function render (x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  const ctx = canvas.getContext('2d')!
  switch (drawing[Value.structKind]) {
    case 'ellipse': {
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      const radiusX = drawing.width / 2
      const radiusY = drawing.height / 2
      const centerX = x + radiusX
      const centerY = y + radiusY
      ctx.beginPath()
      ctx.ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
      if (drawing.mode === 'solid') {
        ctx.fill()
      } else if (drawing.mode === 'outline') {
        ctx.stroke()
      }
      break
    }
    case 'rectangle': {
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      if (drawing.mode === 'solid') {
        ctx.fillRect(x, y, drawing.width, drawing.height)
      } else if (drawing.mode === 'outline') {
        ctx.strokeRect(x, y, drawing.width, drawing.height)
      }
      break
    }
    case 'triangle': {
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      ctx.beginPath()
      // Start in the bottom-left corner of the triangle...
      ctx.moveTo(x, y + drawing.height)
      // Then go to the top corner...
      ctx.lineTo(x + drawing.width / 2, y)
      // And then the bottom-right corner...
      ctx.lineTo(x + drawing.width, y + drawing.height)
      // And back!
      ctx.lineTo(x, y + drawing.height)
      if (drawing.mode === 'solid') {
        ctx.fill()
      } else if (drawing.mode === 'outline') {
        ctx.stroke()
      }
      break
    }
    case 'path': {
      if (drawing.points.length === 0) { break }
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      ctx.beginPath()
      ctx.moveTo(x + drawing.points[0][0], y + drawing.points[0][1])
      drawing.points.slice(1).forEach(p => {
        ctx.lineTo(x + p[0], y + p[1])
      })
      if (drawing.mode === 'solid') {
        ctx.fill()
      } else {
        ctx.stroke()
      }
      break
    }
    case 'beside': {
      drawing.drawings.forEach(d => {
        render(
          x,
          drawing.align === 'top'
            ? y
            : drawing.align === 'bottom'
              ? y + drawing.height - d.height
              // N.B., assumed to be 'center'
              : y + (drawing.height - d.height) / 2,
          d,
          canvas)
        x += d.width
      })
      break
    }
    case 'above': {
      drawing.drawings.forEach(d => {
        render(
          drawing.align === 'left'
            ? x
            : drawing.align === 'right'
              ? x + drawing.width - d.width
              // N.B., assumed to be 'middle'
              : x + (drawing.width - d.width) / 2,
          y,
          d,
          canvas)
        y += d.height
      })
      break
    }
    case 'overlay': {
      // N.B., need to draw in reverse order to get the overlay effect to work
      [...drawing.drawings].reverse().forEach(d => {
        render(
          drawing.xAlign === 'left'
            ? x
            : drawing.xAlign === 'right'
              ? x + drawing.width - d.width
              // N.B., assumed to be 'middle'
              : x + (drawing.width - d.width) / 2,
          drawing.yAlign === 'top'
            ? y
            : drawing.yAlign === 'bottom'
              ? y + drawing.height - d.height
              // N.B., assumed to be 'center'
              : y + (drawing.height - d.height) / 2,
          d,
          canvas)
      })
      break
    }
    case 'overlayOffset': {
      const x1 = drawing.dx > 0 ? x : x + Math.abs(drawing.dx)
      const y1 = drawing.dy > 0 ? y : y + Math.abs(drawing.dy)
      const x2 = drawing.dx > 0 ? x + drawing.dx : x
      const y2 = drawing.dy > 0 ? y + drawing.dy : y
      // N.B., render d2 first so d1 is on top
      render(x2, y2, drawing.d2, canvas)
      render(x1, y1, drawing.d1, canvas)
      break
    }
    case 'rotate': {
      const centerX = x + drawing.width / 2
      const centerY = y + drawing.height / 2
      const angle = drawing.angle * Math.PI / 180
      // N.B., need to move the canvas from the origin to the
      // center of the drawing to rotate and then move back to
      // the origin.
      ctx.translate(centerX, centerY)
      ctx.rotate(angle)
      ctx.translate(-centerX, -centerY);
      // ctx.translate(-drawing.drawing.width / 2, -drawing.drawing.height / 2)
      render(x, y, drawing.drawing, canvas)
      ctx.translate(centerX, centerY)
      // ctx.translate(drawing.drawing.width / 2, drawing.drawing.height / 2)
      ctx.rotate(-angle)
      ctx.translate(-centerX, -centerY)
      break
    }
    case 'withDash': {
      ctx.setLineDash(drawing.dashSpec)
      render(x, y, drawing.drawing, canvas)
      ctx.setLineDash([])
    }
  }
}

function clearDrawing (canvas: HTMLCanvasElement) {
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = 'white'
  ctx.strokeStyle = 'black'
  ctx.fillRect(0, 0, canvas.width, canvas.height)
}

function renderer (drawing: Drawing): HTMLElement {
  const canvas = document.createElement('canvas')
  canvas.width = drawing.width
  canvas.height = drawing.height
  clearDrawing(canvas)
  render(0, 0, drawing, canvas)
  return canvas
}

Render.addCustomWebRenderer(imageQ, renderer)