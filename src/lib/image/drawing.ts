import { checkContract, contract } from '../../contract.js'
import * as C from '../../contract.js'
import * as Render from '../../display.js'
import { emptyLibrary, Library, registerValue, ScamperError, Value } from '../../lang.js'

import { colorNameToRgb, hsvToRgb, Rgb, rgb, rgbAverage, rgbToString } from './rgb.js'

export const lib: Library = emptyLibrary()

/***** Core Functions *********************************************************/

function colorToRgb (v: any): Rgb {
  if (Value.isStructKind(v, 'rgba')) {
    return v
  } else if (typeof v === 'string') {
    return colorNameToRgb(v)
  } else if (Value.isStructKind(v, 'hsv')) {
    return hsvToRgb(v)
  } else {
    throw new ScamperError('Runtime', `Shapes expect a valid color, received a: ${Value.typeOf(v)}`)
  }
}

// N.B., color is a legacy function from the htdp library. Currently, we standardize
// on Rgb as the stored color type for shapes.
function color (r: number, g: number, b: number, a: number): Rgb {
  checkContract(arguments, contract('color', [C.nonneg, C.nonneg, C.nonneg, C.nonneg]))
  return rgb(r, g, b, a)
}
registerValue('color', color, lib)

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

// N.B., colors through the drawing/shape API can be a variety of inputs (ugh).
// It is the responsibility of constructor functions to convert these into
// Rgb values internally.
const colorS: C.Spec = {
  // https://stackoverflow.com/questions/48484767/javascript-check-if-string-is-valid-css-color
  predicate: (v: any) => typeof v === 'string' || Value.isStructKind(v, 'rgba') || Value.isStructKind(v, 'hsv'),
  errorMsg: (actual: any) => `expected a color, received ${Value.typeOf(actual)}`
}

type Mode = 'solid' | 'outline'
export type Drawing = Ellipse | Rectangle | Triangle | Path | Beside | Above | Overlay | OverlayOffset | Rotate | WithDash

function drawingQ (v: any): boolean {
  checkContract(arguments, contract('image?', [C.any]))
  return Value.isStructKind(v, 'ellipse') || Value.isStructKind(v, 'rectangle') ||
         Value.isStructKind(v, 'triangle') || Value.isStructKind(v, 'path') ||
         Value.isStructKind(v, 'beside') || Value.isStructKind(v, 'above') ||
         Value.isStructKind(v, 'overlay') || Value.isStructKind(v, 'overlayOffset') ||
         Value.isStructKind(v, 'rotate') || Value.isStructKind(v, 'withDash')
}
registerValue('image?', drawingQ, lib)
// TODO: in the new 151 library, images generalize to more than just shapes!
// In particular, images include shapes, image files, etc. We don't have
// such a unified view in Scamper (yet), so for now, shape? is an alias of
// image?
registerValue('shape?', drawingQ, lib)

const drawingS = {
  predicate: drawingQ,
  errorMsg: (actual: any) => `expected a drawing, received ${Value.typeOf(actual)}`
}

interface Ellipse extends Value.Struct {
  [Value.structKind]: 'ellipse',
  width: number,
  height: number,
  mode: Mode,
  color: Rgb
}

const ellipsePrim = (width: number, height: number, mode: Mode, color: any): Ellipse => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'ellipse',
  width, height, mode, color: colorToRgb(color)
})

function ellipse (width: number, height: number, mode: Mode, color: any): Ellipse {
  checkContract(arguments, contract('ellipse', [C.nonneg, C.nonneg, modeS, colorS]))
  return ellipsePrim(width, height, mode, color)
}
registerValue('ellipse', ellipse, lib)

function circle (radius: number, mode: Mode, color: any): Ellipse {
  checkContract(arguments, contract('circle', [C.nonneg, modeS, colorS]))
  return ellipsePrim(radius * 2, radius * 2, mode, color)
}
registerValue('circle', circle, lib)

interface Rectangle extends Value.Struct {
  [Value.structKind]: 'rectangle',
  width: number,
  height: number,
  mode: Mode,
  color: Rgb
}

const rectanglePrim = (width: number, height: number, mode: Mode, color: any): Rectangle => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'rectangle',
  width, height, mode, color: colorToRgb(color)
})

function rectangle (width: number, height: number, mode: Mode, color: any): Rectangle {
  checkContract(arguments, contract('rectangle', [C.nonneg, C.nonneg, modeS, colorS]))
  return rectanglePrim(width, height, mode, color)
}
registerValue('rectangle', rectangle, lib)

function square (length: number, mode: Mode, color: any): Rectangle {
  checkContract(arguments, contract('square', [C.nonneg, modeS, colorS]))
  return rectanglePrim(length, length, mode, color)
}
registerValue('square', square, lib)

interface Triangle extends Value.Struct {
  [Value.structKind]: 'triangle',
  width: number,
  height: number,
  mode: Mode,
  color: Rgb
}

const trianglePrim = (width: number, height: number, mode: Mode, color: any): Triangle => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'triangle',
  width, height, mode, color: colorToRgb(color)
})

function triangle (length: number, mode: Mode, color: any): Triangle {
  checkContract(arguments, contract('triangle', [C.nonneg, modeS, colorS]))
  return trianglePrim(length, length * Math.sqrt(3) / 2, mode, color)
}
registerValue('triangle', triangle, lib)

function isoscelesTriangle (width: number, height: number, mode: Mode, color: any): Triangle {
  checkContract(arguments, contract('isosceles-triangle', [C.nonneg, C.nonneg, modeS, colorS]))
  return trianglePrim(width, height, mode, color)
}
registerValue('isosceles-triangle', isoscelesTriangle, lib)

interface Path extends Value.Struct {
  [Value.structKind]: 'path',
  width: number,
  height: number,
  points: [number, number][],
  mode: Mode,
  color: Rgb
}

const pathPrim = (width: number, height: number, points: [number, number][], mode: Mode, color: any): Path => ({
  [Value.scamperTag]: 'struct', [Value.structKind]: 'path',
  width, height, points, mode, color: colorToRgb(color)
})

function path (width: number, height: number, points: Value.List, mode: Mode, color: any): Path {
  checkContract(arguments, contract('path', [C.nonneg, C.nonneg, C.list, modeS, colorS]))
  return pathPrim(width, height, 
    Value.listToVector(points).map((p: Value.T) => [(p as Value.Pair).fst, (p as Value.Pair).snd]) as [number, number][],
    mode, color)
}
registerValue('path', path, lib)

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
  checkContract(arguments, contract('beside', [], drawingS)) 
  return besideAlignPrim('center', ...drawings)
}
registerValue('beside', beside, lib)

function besideAlign (align: string, ...drawings: Drawing[]): Beside {
  checkContract(arguments, contract('beside/align', [alignVS], drawingS))
  return besideAlignPrim(align, ...drawings)
}
registerValue('beside/align', besideAlign, lib)

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
  checkContract(arguments, contract('above', [], drawingS))
  return aboveAlignPrim('middle', ...drawings)
}
registerValue('above', above, lib)

function aboveAlign (align: string, ...drawings: Drawing[]): Above {
  checkContract(arguments, contract('above/align', [alignHS], drawingS))
  return aboveAlignPrim(align, ...drawings)
}
registerValue('above/align', aboveAlign, lib)

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
  checkContract(arguments, contract('overlay', [], drawingS))
  return overlayAlignPrim('middle', 'center', ...drawings)
}
registerValue('overlay', overlay, lib)

function overlayAlign (xAlign: string, yAlign: string, ...drawings: Drawing[]): Overlay {
  checkContract(arguments, contract('overlay/align', [alignHS, alignVS], drawingS))
  return overlayAlignPrim(xAlign, yAlign, ...drawings)
}
registerValue('overlay/align', overlayAlign, lib)

interface OverlayOffset extends Value.Struct {
  [Value.structKind]: 'overlayOffset',
  dx: number,
  dy: number,
  width: number,
  height: number,
  d1: Drawing,
  d2: Drawing
}

function overlayOffsetPrim (dx: number, dy: number, width: number, height: number, d1: Drawing, d2: Drawing): OverlayOffset {
  return {
    [Value.scamperTag]: 'struct', [Value.structKind]: 'overlayOffset',
    dx,
    dy,
    width,
    height,
    d1,
    d2
  }
}

function overlayOffset (dx: number, dy: number, d1: Drawing, d2: Drawing): OverlayOffset {
  checkContract(arguments, contract('overlay/offset', [C.number, C.number, drawingS, drawingS]))
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
  // BUG: what if d2 is actually bigger than d1? Then the calculation needs to mirror!
  return overlayOffsetPrim(dx, dy, width, height, d1, d2)
}
registerValue('overlay/offset', overlayOffset, lib)

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
  checkContract(arguments, contract('rotate', [C.number, drawingS]))
  const dims = calculateRotatedBox(drawing.width, drawing.height, angle)
  return {
    [Value.scamperTag]: 'struct', [Value.structKind]: 'rotate',
    width: dims.width,
    height: dims.height,
    angle,
    drawing
  }
}
registerValue('rotate', rotate, lib)

interface WithDash extends Value.Struct {
  [Value.structKind]: 'withDash',
  dashSpec: number[],
  drawing: Drawing,
  width: number,
  height: number
}

function withDash (dashSpec: number[], drawing: Drawing): WithDash {
  checkContract(arguments, contract('with-dash', [C.list, drawingS])) 
  return {
    [Value.scamperTag]: 'struct', [Value.structKind]: 'withDash',
    dashSpec,
    drawing,
    width: drawing.width,
    height: drawing.height
  }
}
registerValue('with-dash', withDash, lib)

/***** Extended Functions *****************************************************/

function solidSquare(length: number, color: any): Rectangle {
  return square(length, 'solid', color)
}

function outlinedSquare(length: number, color: any): Rectangle {
  return square(length, 'outline', color)
}

function solidRectangle(width: number, height: number, color: any): Rectangle {
  return rectangle(width, height, 'solid', color)
}

function outlinedRectangle(width: number, height: number, color: any): Rectangle {
  return rectangle(width, height, 'outline', color)
}

function solidCircle(radius: number, color: any): Ellipse {
  return circle(radius, 'solid', color)
}

function outlinedCircle(radius: number, color: any): Ellipse {
  return circle(radius, 'outline', color)
}

function solidEllipse(width: number, height: number, color: any): Ellipse {
  return ellipse(width, height, 'solid', color)
}

function outlinedEllipse(width: number, height: number, color: any): Ellipse {
  return ellipse(width, height, 'outline', color)
}

function solidTriangle(length: number, color: any): Triangle {
  return triangle(length, 'solid', color)
}

function outlinedTriangle(length: number, color: any): Triangle {
  return triangle(length, 'outline', color)
}

function solidIsoscelesTriangle(width: number, height: number, color: any): Triangle {
  return isoscelesTriangle(width, height, 'solid', color)
}

function outlinedIsoscelesTriangle(width: number, height: number, color: any): Triangle {
  return isoscelesTriangle(width, height, 'outline', color)
}

function imageWidth (drawing: Drawing): number {
  checkContract(arguments, contract('image-width', [drawingS]))
  return drawing.width
}

function imageHeight (drawing: Drawing): number {
  checkContract(arguments, contract('image-height', [drawingS]))
  return drawing.height
}

function imageColor (drawing: Drawing): Rgb {
  checkContract(arguments, contract('image-color', [drawingS]))
  switch(drawing[Value.structKind]) {
    case 'ellipse':
    case 'rectangle':
    case 'triangle':
    case 'path':
      return drawing.color
    // N.B.: what do we return for aggregates, the average color?
    case 'beside':
    case 'above':
    case 'overlay': {
      var avg = imageColor(drawing.drawings[0])
      for (var i = 1; i < drawing.drawings.length; i++) {
        avg = rgbAverage(avg, imageColor(drawing.drawings[i]))
      }
      return avg
    }
    case 'overlayOffset':
      return rgbAverage(imageColor(drawing.d1), imageColor(drawing.d2))
    case 'rotate':
      return imageColor(drawing.drawing)
    case 'withDash':
      return imageColor(drawing.drawing)
  }
}

function imageRecolor (drawing: Drawing, color: any): Drawing {
  checkContract(arguments, contract('image-recolor', [drawingS, colorS]))
  switch(drawing[Value.structKind]) {
    case 'ellipse':
      return ellipsePrim(drawing.width, drawing.height, drawing.mode, color)
    case 'rectangle':
      return rectanglePrim(drawing.width, drawing.height, drawing.mode, color)
    case 'triangle':
      return trianglePrim(drawing.width, drawing.height, drawing.mode, color)
    case 'path':
      return pathPrim(drawing.width, drawing.height, drawing.points, drawing.mode, color)
    case 'beside':
      return besideAlignPrim(drawing.align, ...drawing.drawings.map(d => imageRecolor(d, color)))
    case 'above':
      return aboveAlignPrim(drawing.align, ...drawing.drawings.map(d => imageRecolor(d, color)))
    case 'overlay':
      return overlayAlignPrim(drawing.xAlign, drawing.yAlign, ...drawing.drawings.map(d => imageRecolor(d, color)))
    case 'overlayOffset':
      return overlayOffsetPrim(drawing.dx, drawing.dy, drawing.width, drawing.height, imageRecolor(drawing.d1, color), imageRecolor(drawing.d2, color))
    case 'rotate':
      return rotate(drawing.angle, imageRecolor(drawing.drawing, color))
    case 'withDash':
      return withDash(drawing.dashSpec, imageRecolor(drawing.drawing, color))
  }
}

registerValue('solid-square', solidSquare, lib)
registerValue('outlined-square', outlinedSquare, lib)
registerValue('solid-rectangle', solidRectangle, lib)
registerValue('outlined-rectangle', outlinedRectangle, lib)
registerValue('solid-circle', solidCircle, lib)
registerValue('outlined-circle', outlinedCircle, lib)
registerValue('solid-ellipse', solidEllipse, lib)
registerValue('outlined-ellipse', outlinedEllipse, lib)
registerValue('solid-triangle', solidTriangle, lib)
registerValue('outlined-triangle', outlinedTriangle, lib)
registerValue('solid-isosceles-triangle', solidIsoscelesTriangle, lib)
registerValue('outlined-isosceles-triangle', outlinedIsoscelesTriangle, lib)

registerValue('image-width', imageWidth, lib)
registerValue('image-height', imageHeight, lib)
registerValue('image-color', imageColor, lib)
registerValue('image-recolor', imageRecolor, lib)

/***** Rendering **************************************************************/

export function render (x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  const ctx = canvas.getContext('2d')!
  switch (drawing[Value.structKind]) {
    case 'ellipse': {
      ctx.fillStyle = rgbToString(drawing.color)
      ctx.strokeStyle = rgbToString(drawing.color)
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
      ctx.fillStyle = rgbToString(drawing.color)
      ctx.strokeStyle = rgbToString(drawing.color)
      if (drawing.mode === 'solid') {
        ctx.fillRect(x, y, drawing.width, drawing.height)
      } else if (drawing.mode === 'outline') {
        ctx.strokeRect(x, y, drawing.width, drawing.height)
      }
      break
    }
    case 'triangle': {
      ctx.fillStyle = rgbToString(drawing.color)
      ctx.strokeStyle = rgbToString(drawing.color)
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
      ctx.fillStyle = rgbToString(drawing.color)
      ctx.strokeStyle = rgbToString(drawing.color)
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

Render.addCustomWebRenderer(drawingQ, renderer)