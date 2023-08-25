import * as Render from '../display.js'
import * as V from '../value.js'

function color (r: number, g: number, b: number, a: number): string {
  return `rgba(${r}, ${g}, ${b}, ${a})`
}

type Mode = 'solid' | 'outline'
export type Drawing = Ellipse | Rectangle | Triangle | Path | Beside | Above | Overlay | OverlayOffset | Rotate | WithDash

const imageQ = (v: any): boolean => typeof v === 'object' && v._scamperTag === 'image'

type Ellipse = { _scamperTag: 'image', kind: 'ellipse', width: number, height: number, mode: Mode, color: string }
const ellipse = (width: number, height: number, mode: Mode, color: string): Ellipse => ({
  _scamperTag: 'image', kind: 'ellipse', width, height, mode, color
})

const circle = (radius: number, mode: Mode, color: string): Ellipse => ellipse(radius * 2, radius * 2, mode, color)

type Rectangle = { _scamperTag: 'image', kind: 'rectangle', width: number, height: number, mode: Mode, color: string }
const rectangle = (width: number, height: number, mode: Mode, color: string): Rectangle => ({
  _scamperTag: 'image', kind: 'rectangle', width, height, mode, color
})

const square = (length: number, mode: Mode, color: string): Rectangle => rectangle(length, length, mode, color)

type Triangle = { _scamperTag: 'image', kind: 'triangle', width: number, height: number, length: number, mode: Mode, color: string }
const triangle = (length: number, mode: Mode, color: string): Triangle => ({
  _scamperTag: 'image', kind: 'triangle', width: length, height: length * Math.sqrt(3) / 2,
  length, mode, color
})

type Path = { _scamperTag: 'image', kind: 'path', width: number, height: number, points: [number, number][], mode: Mode, color: string }
const path = (width: number, height: number, points: [number, number][], mode: Mode, color: string): Path =>
  ({ _scamperTag: 'image', kind: 'path', width, height, points, mode, color })


type Beside = { _scamperTag: 'image', kind: 'beside', align: string, width: number, height: number, drawings: Drawing[] }
const besideAlign = (align: string, ...drawings: Drawing[]): Beside => ({
  _scamperTag: 'image',
  kind: 'beside',
  align,
  width: drawings.reduce((acc, d) => acc + d.width, 0),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

const beside = (...drawings: Drawing[]): Beside => besideAlign('center', ...drawings)

type Above = { _scamperTag: 'image', kind: 'above', align: string, width: number, height: number, drawings: Drawing[] }
const aboveAlign = (align: string, ...drawings: Drawing[]): Above => ({
  _scamperTag: 'image',
  kind: 'above',
  align,
  width: Math.max(...drawings.map(d => d.width)),
  height: drawings.reduce((acc, d) => acc + d.height, 0),
  drawings
})

const above = (...drawings: Drawing[]): Above => aboveAlign('middle', ...drawings)

type Overlay = { _scamperTag: 'image', kind: 'overlay', xAlign: string, yAlign: string, width: number, height: number, drawings: Drawing[] }
const overlayAlign = (xAlign: string, yAlign: string, ...drawings: Drawing[]): Overlay => ({
  _scamperTag: 'image',
  kind: 'overlay',
  xAlign,
  yAlign,
  width: Math.max(...drawings.map(d => d.width)),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

const overlay = (...drawings: Drawing[]) => overlayAlign('middle', 'center', ...drawings)

type OverlayOffset = { _scamperTag: 'image', kind: 'overlayOffset', dx: number, dy: number, width: number, height: number, d1: Drawing, d2: Drawing }
const overlayOffset = (dx: number, dy: number, d1: Drawing, d2: Drawing): OverlayOffset => {
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
    _scamperTag: 'image',
    kind: 'overlayOffset',
    dx,
    dy,
    // BUG: what if d2 is actually bigger than d1? Then the calculation needs to mirror!
    width,
    height,
    d1,
    d2
  }
}

type Rotate = { _scamperTag: 'image', kind: 'rotate', width: number, height: number, angle: number, drawing: Drawing }
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

const rotate = (angle: number, drawing: Drawing): Rotate => {
  const dims = calculateRotatedBox(drawing.width, drawing.height, angle)
  return {
    _scamperTag: 'image',
    kind: 'rotate',
    width: dims.width,
    height: dims.height,
    angle,
    drawing
  }
}

type WithDash = { _scamperTag: 'image', kind: 'withDash', dashSpec: number[], drawing: Drawing, width: number, height: number }
const withDash = (dashSpec: number[], drawing: Drawing): WithDash => ({
  _scamperTag: 'image',
  kind: 'withDash',
  dashSpec,
  drawing,
  width: drawing.width,
  height: drawing.height
})

export const imageLib: [string, V.Value][] = [
  ['image?', V.mkJsFunction(imageQ, 1)],
  ['color', V.mkJsFunction(color, 4)],
  ['ellipse', V.mkJsFunction(ellipse, 4)],
  ['circle', V.mkJsFunction(circle, 3)],
  ['rectangle', V.mkJsFunction(rectangle, 4)],
  ['square', V.mkJsFunction(square, 3)],
  ['triangle', V.mkJsFunction(triangle, 3)],
  ['path', V.mkJsFunction(path, 5)],
  ['beside', V.mkJsFunction(beside, 1, true)],
  ['beside/align', V.mkJsFunction(besideAlign, 2, true)],
  ['above', V.mkJsFunction(above, 1, true)],
  ['above/align', V.mkJsFunction(aboveAlign, 2, true)],
  ['overlay', V.mkJsFunction(overlay, 1, true)],
  ['overlay/align', V.mkJsFunction(overlayAlign, 3, true)],
  ['overlay/offset', V.mkJsFunction(overlayOffset, 4)],
  ['rotate', V.mkJsFunction(rotate, 2)],
  ['with-dash', V.mkJsFunction(withDash, 2)]
]

export function render (x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  const ctx = canvas.getContext('2d')!
  switch (drawing.kind) {
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
      ctx.translate(-centerX, -centerY)
      render(x, y, drawing.drawing, canvas)
      ctx.translate(centerX, centerY)
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