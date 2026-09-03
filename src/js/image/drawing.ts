import * as L from '../../lpm'
import { Rgb, color_rgb, color_colorToRgb, color_rgbAverage, color_rgbToString } from './color.js'
import { Font, font_font, font_fontQ, font_fontToFontString } from './font.js'
import { context2d } from './context.js'

/***** Core Functions *********************************************************/

type Mode = 'solid' | 'outline'

/** How wide an outlined shape is stroked when it is not told (#431). */
const defaultLineWidth = 1

/**
 * The width an outlined shape strokes with; undefined for a solid one, which
 * has no stroke and so carries no `lineWidth` field at all.
 * @throws ScamperError if the width is not positive: the canvas ignores a
 *         width of zero or less, leaving whatever the last shape stroked with,
 *         and a negative one would shrink the box below the size asked for.
 */
function strokeWidth(mode: Mode, lineWidth?: number): number | undefined {
  if (mode !== 'outline') { return undefined }
  const width = lineWidth ?? defaultLineWidth
  if (width <= 0) {
    throw new L.ScamperError('Runtime', `expected a positive line width, received ${width.toString()}`)
  }
  return width
}

/**
 * The fields every stroke-able shape shares: its box, which is the size it was
 * given plus its line width, since the stroke is centred on that size and so
 * half of it falls outside (#431); and the stroke itself, present only when
 * there is one.
 */
function shapeFields(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number) {
  const stroke = strokeWidth(mode, lineWidth)
  return {
    width: width + (stroke ?? 0),
    height: height + (stroke ?? 0),
    mode,
    color: color_colorToRgb(color),
    ...(stroke !== undefined ? { lineWidth: stroke } : {})
  }
}

export type Drawing = Ellipse | Rectangle | Triangle | Path | Beside | Above | Overlay | OverlayOffset | Rotate | WithDash | DText

/** A fill mode: the string "solid" or "outline". */
export function drawing_fillModeQ (v: L.Value): boolean {
  return v === 'solid' || v === 'outline'
}

export function drawing_drawingQ (v: L.Value): boolean {
  return L.isStructKind(v, 'ellipse') || L.isStructKind(v, 'rectangle') ||
         L.isStructKind(v, 'triangle') || L.isStructKind(v, 'path') ||
         L.isStructKind(v, 'beside') || L.isStructKind(v, 'above') ||
         L.isStructKind(v, 'overlay') || L.isStructKind(v, 'overlayOffset') ||
         L.isStructKind(v, 'rotate') || L.isStructKind(v, 'withDash') ||
         L.isStructKind(v, 'text')
}
// TODO: in the new 151 library, images generalize to more than just shapes!
// In particular, images include shapes, image files, etc. We don't have
// such a unified view in Scamper (yet), so for now, shape? is an alias of
// image?

interface Ellipse extends L.Struct {
  [L.structKind]: 'ellipse',
  width: number,
  height: number,
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

const ellipsePrim = (width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number): Ellipse => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'ellipse',
  ...shapeFields(width, height, mode, color, lineWidth)
})

export function drawing_ellipse(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number): Ellipse {
  return ellipsePrim(width, height, mode, color, lineWidth)
}

/**
 * A circle is sized by its diameter, not its radius, so that the one number it
 * takes means what `square`'s does: `(circle d ...)` is `d` across (#433).
 */
export function drawing_circle(diameter: number, mode: Mode, color: L.Value, lineWidth?: number): Ellipse {
  return ellipsePrim(diameter, diameter, mode, color, lineWidth)
}

interface Rectangle extends L.Struct {
  [L.structKind]: 'rectangle',
  width: number,
  height: number,
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

const rectanglePrim = (width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number): Rectangle => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'rectangle',
  ...shapeFields(width, height, mode, color, lineWidth)
})

export function drawing_rectangle(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number): Rectangle {
  return rectanglePrim(width, height, mode, color, lineWidth)
}

export function drawing_square(length: number, mode: Mode, color: L.Value, lineWidth?: number): Rectangle {
  return rectanglePrim(length, length, mode, color, lineWidth)
}

interface Triangle extends L.Struct {
  [L.structKind]: 'triangle',
  width: number,
  height: number,
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

const trianglePrim = (width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number): Triangle => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'triangle',
  ...shapeFields(width, height, mode, color, lineWidth)
})

export function drawing_triangle(length: number, mode: Mode, color: L.Value, lineWidth?: number): Triangle {
  return trianglePrim(length, length * Math.sqrt(3) / 2, mode, color, lineWidth)
}

export function drawing_isoscelesTriangle(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number): Triangle {
  return trianglePrim(width, height, mode, color, lineWidth)
}

interface Path extends L.Struct {
  [L.structKind]: 'path',
  width: number,
  height: number,
  points: [number, number][],
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

// N.B., `points` sits between the box and the mode, matching the argument
// order of `path` itself, so the fields shapeFields supplies are spread on
// either side of it rather than all at the end.
const pathPrim = (width: number, height: number, points: [number, number][], mode: Mode, color: L.Value, lineWidth?: number): Path => {
  const { width: w, height: h, ...rest } = shapeFields(width, height, mode, color, lineWidth)
  return {
    [L.scamperTag]: 'struct', [L.structKind]: 'path',
    width: w, height: h, points, ...rest
  }
}

export function drawing_path(width: number, height: number, points: L.List, mode: Mode, color: L.Value, lineWidth?: number): Path {
  return pathPrim(width, height,
    L.listToVector(points).map((p: L.Value) => [(p as L.Pair).fst, (p as L.Pair).snd]) as [number, number][],
    mode, color, lineWidth)
}

interface Beside extends L.Struct {
  [L.structKind]: 'beside',
  align: string,
  width: number,
  height: number,
  drawings: Drawing[]
}

const besideAlignPrim = (align: string, ...drawings: Drawing[]): Beside => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'beside',
  align,
  width: drawings.reduce((acc, d) => acc + d.width, 0),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

export function drawing_beside(...drawings: Drawing[]): Beside {
  return besideAlignPrim('center', ...drawings)
}

export function drawing_besideAlign(align: string, ...drawings: Drawing[]): Beside {
  return besideAlignPrim(align, ...drawings)
}

interface Above extends L.Struct {
  [L.structKind]: 'above',
  align: string,
  width: number,
  height: number,
  drawings: Drawing[]
}

const aboveAlignPrim = (align: string, ...drawings: Drawing[]): Above => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'above',
  align,
  width: Math.max(...drawings.map(d => d.width)),
  height: drawings.reduce((acc, d) => acc + d.height, 0),
  drawings
})

export function drawing_above(...drawings: Drawing[]): Above {
  return aboveAlignPrim('middle', ...drawings)
}

export function drawing_aboveAlign(align: string, ...drawings: Drawing[]): Above {
  return aboveAlignPrim(align, ...drawings)
}

interface Overlay extends L.Struct {
  [L.structKind]: 'overlay',
  xAlign: string,
  yAlign: string,
  width: number,
  height: number,
  drawings: Drawing[]
}

const overlayAlignPrim = (xAlign: string, yAlign: string, ...drawings: Drawing[]): Overlay => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'overlay',
  xAlign,
  yAlign,
  width: Math.max(...drawings.map(d => d.width)),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

export function drawing_overlay(...drawings: Drawing[]) {
  return overlayAlignPrim('middle', 'center', ...drawings)
}

export function drawing_overlayAlign(xAlign: string, yAlign: string, ...drawings: Drawing[]): Overlay {
  return overlayAlignPrim(xAlign, yAlign, ...drawings)
}

interface OverlayOffset extends L.Struct {
  [L.structKind]: 'overlayOffset',
  dx: number,
  dy: number,
  width: number,
  height: number,
  d1: Drawing,
  d2: Drawing
}

function overlayOffsetPrim (dx: number, dy: number, width: number, height: number, d1: Drawing, d2: Drawing): OverlayOffset {
  return {
    [L.scamperTag]: 'struct', [L.structKind]: 'overlayOffset',
    dx,
    dy,
    width,
    height,
    d1,
    d2
  }
}

export function drawing_overlayOffset(dx: number, dy: number, d1: Drawing, d2: Drawing): OverlayOffset {
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

interface Rotate extends L.Struct {
  [L.structKind]: 'rotate',
  width: number,
  height: number,
  dx: number,
  dy: number,
  angle: number,
  drawing: Drawing
}

function calculateRotatedBox (points: [number, number][], degrees: number): { width: number, height: number, dx: number, dy: number } {
  // Calculate the rotated points
  const angle = degrees * Math.PI / 180
  const rotatedPoints = points.map(
    ([x, y]) => [
      x * Math.cos(angle) - y * Math.sin(angle),
      x * Math.sin(angle) + y * Math.cos(angle)
    ]
  )

  // Determine the width and height of the bounding
  // box by taking mins and maxes of the points.
  const xMin = Math.min(...rotatedPoints.map(([x, _]) => x))
  const xMax = Math.max(...rotatedPoints.map(([x, _]) => x))
  const yMin = Math.min(...rotatedPoints.map(([_, y]) => y))
  const yMax = Math.max(...rotatedPoints.map(([_, y]) => y))

  return {
    width: xMax - xMin,
    height: yMax - yMin,
    dx: -xMin,
    dy: -yMin
  }
}

export function drawing_rotate(angle: number, drawing: Drawing): Rotate {
  // Rotate the drawing's declared bounding-box corners. At angle 0 this is the
  // identity for every shape (box = w x h, dx = dy = 0), so `rotate` never
  // shifts, clips, or resizes a drawing it isn't actually turning.
  const corners: [number, number][] = [
    [0, 0],
    [drawing.width, 0],
    [drawing.width, drawing.height],
    [0, drawing.height]
  ]
  const dims = calculateRotatedBox(corners, angle)
  return {
    [L.scamperTag]: 'struct', [L.structKind]: 'rotate',
    width: dims.width,
    height: dims.height,
    dx: dims.dx,
    dy: dims.dy,
    angle,
    drawing
  }
}

interface WithDash extends L.Struct {
  [L.structKind]: 'withDash',
  dashSpec: number[],
  drawing: Drawing,
  width: number,
  height: number
}

export function drawing_withDash(dashSpec: number[], drawing: Drawing): WithDash {
  return {
    [L.scamperTag]: 'struct', [L.structKind]: 'withDash',
    dashSpec,
    drawing,
    width: drawing.width,
    height: drawing.height
  }
}
interface DText extends L.Struct {
  [L.structKind]: 'text',
  width: number,
  height: number,
  text: string,
  size: number,
  color: Rgb
  font: Font,
}

function textPrim (width: number, height: number, text: string,
    font: Font, size: number, color: L.Value): DText {
  return {
    [L.scamperTag]: 'struct', [L.structKind]: 'text',
    width, height, text, size, color: color_colorToRgb(color), font
  }
}

export function drawing_text(text: string, size: number, color: Rgb, ...rest: L.Value[]): DText {
  let f: Font = font_font('Arial')
  if (rest.length > 1) {
    throw new L.ScamperError('Runtime', `wrong number of arguments to text provided. Expected 3 or 4, received ${3 + rest.length}.`)
  } else if (rest.length == 1 && font_fontQ(rest[0])) {
    if (font_fontQ(rest[0])) {
      f = rest[0] as Font
    } else {
      throw new L.ScamperError('Runtime', `expected a font, received ${L.typeOf(rest[0])}`)
    }
  }

  // N.B., to calculate the width and height of text, we need to make a
  // temporary canvas to measure the text's dimensions.
  const canvas = document.createElement('canvas')
  const ctx = context2d(canvas)
  ctx.font = font_fontToFontString(f, size)
  console.log(font_fontToFontString(f, size))
  const met = ctx.measureText(text)
  const width = met.width
  const height = met.actualBoundingBoxAscent + met.actualBoundingBoxDescent + 1

  return textPrim(width, height, text, f, size, color)
}

/***** Extended Functions *****************************************************/

export function drawing_solidSquare(length: number, color: L.Value): Rectangle {
  return drawing_square(length, 'solid', color)
}

export function drawing_outlinedSquare(length: number, color: L.Value, lineWidth?: number): Rectangle {
  return drawing_square(length, 'outline', color, lineWidth)
}

export function drawing_solidRectangle(width: number, height: number, color: L.Value): Rectangle {
  return drawing_rectangle(width, height, 'solid', color)
}

export function drawing_outlinedRectangle(width: number, height: number, color: L.Value, lineWidth?: number): Rectangle {
  return drawing_rectangle(width, height, 'outline', color, lineWidth)
}

export function drawing_solidCircle(diameter: number, color: L.Value): Ellipse {
  return drawing_circle(diameter, 'solid', color)
}

export function drawing_outlinedCircle(diameter: number, color: L.Value, lineWidth?: number): Ellipse {
  return drawing_circle(diameter, 'outline', color, lineWidth)
}

export function drawing_solidEllipse(width: number, height: number, color: L.Value): Ellipse {
  return drawing_ellipse(width, height, 'solid', color)
}

export function drawing_outlinedEllipse(width: number, height: number, color: L.Value, lineWidth?: number): Ellipse {
  return drawing_ellipse(width, height, 'outline', color, lineWidth)
}

export function drawing_solidTriangle(length: number, color: L.Value): Triangle {
  return drawing_triangle(length, 'solid', color)
}

export function drawing_outlinedTriangle(length: number, color: L.Value, lineWidth?: number): Triangle {
  return drawing_triangle(length, 'outline', color, lineWidth)
}

export function drawing_solidIsoscelesTriangle(width: number, height: number, color: L.Value): Triangle {
  return drawing_isoscelesTriangle(width, height, 'solid', color)
}

export function drawing_outlinedIsoscelesTriangle(width: number, height: number, color: L.Value, lineWidth?: number): Triangle {
  return drawing_isoscelesTriangle(width, height, 'outline', color, lineWidth)
}

// TODO: this need to be factored out to a general image lib that handles both
// drawings and canvases.

export function drawing_drawingWidth(drawing: Drawing): number {
  if (drawing_drawingQ(drawing)) {
    return drawing.width
  } else {
    return (drawing as unknown as HTMLCanvasElement).width
  }
}

export function drawing_drawingHeight(drawing: Drawing): number {
  if (drawing_drawingQ(drawing)) {
    return drawing.height
  } else {
    return (drawing as unknown as HTMLCanvasElement).height
  }
}

export function drawing_drawingColor(drawing: Drawing): Rgb {
  switch(drawing[L.structKind]) {
    case 'ellipse':
    case 'rectangle':
    case 'triangle':
    case 'path':
      return drawing.color
    // N.B.: what do we return for aggregates, the average color?
    case 'beside':
    case 'above':
    case 'overlay': {
      let avg = drawing_drawingColor(drawing.drawings[0])
      for (let i = 1; i < drawing.drawings.length; i++) {
        avg = color_rgbAverage(avg, drawing_drawingColor(drawing.drawings[i]))
      }
      return avg
    }
    case 'overlayOffset':
      return color_rgbAverage(drawing_drawingColor(drawing.d1), drawing_drawingColor(drawing.d2))
    case 'rotate':
      return drawing_drawingColor(drawing.drawing)
    case 'withDash':
      return drawing_drawingColor(drawing.drawing)
    case 'text':
      return drawing.color
  }
}

/**
 * The size a shape was built from: its box less the line width the box was
 * grown by, so that rebuilding it (recolor) neither grows it again nor loses
 * the stroke it was drawn with.
 */
function shapeSize(drawing: Strokeable & { width: number, height: number }): [number, number] {
  const lineWidth = drawing.lineWidth ?? 0
  return [drawing.width - lineWidth, drawing.height - lineWidth]
}

export function drawing_drawingRecolor(drawing: Drawing, color: L.Value): Drawing {
  switch(drawing[L.structKind]) {
    case 'ellipse':
      return ellipsePrim(...shapeSize(drawing), drawing.mode, color, drawing.lineWidth)
    case 'rectangle':
      return rectanglePrim(...shapeSize(drawing), drawing.mode, color, drawing.lineWidth)
    case 'triangle':
      return trianglePrim(...shapeSize(drawing), drawing.mode, color, drawing.lineWidth)
    case 'path': {
      const [width, height] = shapeSize(drawing)
      return pathPrim(width, height, drawing.points, drawing.mode, color, drawing.lineWidth)
    }
    case 'beside':
      return besideAlignPrim(drawing.align, ...drawing.drawings.map(d => drawing_drawingRecolor(d, color)))
    case 'above':
      return aboveAlignPrim(drawing.align, ...drawing.drawings.map(d => drawing_drawingRecolor(d, color)))
    case 'overlay':
      return overlayAlignPrim(drawing.xAlign, drawing.yAlign, ...drawing.drawings.map(d => drawing_drawingRecolor(d, color)))
    case 'overlayOffset':
      return overlayOffsetPrim(drawing.dx, drawing.dy, drawing.width, drawing.height, drawing_drawingRecolor(drawing.d1, color), drawing_drawingRecolor(drawing.d2, color))
    case 'rotate':
      return drawing_rotate(drawing.angle, drawing_drawingRecolor(drawing.drawing, color))
    case 'withDash':
      return drawing_withDash(drawing.dashSpec, drawing_drawingRecolor(drawing.drawing, color))
    case 'text':
      return textPrim(drawing.width, drawing.height, drawing.text,
        drawing.font, drawing.size, drawing.color)
  }
}

export function drawing_drawingToPixels(drawing: Drawing): Rgb[] {
  const canvas = drawing_renderer(drawing) as HTMLCanvasElement
  const ctx = context2d(canvas)
  const src = ctx.getImageData(0, 0, canvas.width, canvas.height).data
  const ret = []
  for (let i = 0; i < src.length; i += 4) {
    ret.push(color_rgb(src[i], src[i + 1], src[i + 2], src[i + 3]))
  }
  return ret
}

export function drawing_drawingToCanvas(drawing: Drawing): HTMLCanvasElement {
  return drawing_renderer(drawing) as HTMLCanvasElement
}

/***** Rendering **************************************************************/

/** A shape carrying a stroke: everything drawn by fillOrStroke below. */
interface Strokeable {
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

/**
 * Sets the colours and the stroke for one shape.
 * @param join how the stroke turns a corner. A right angle's miter reaches
 *        exactly the box's corner, so a rectangle keeps its square corners;
 *        an angle as sharp as a triangle's apex would spike far past the box,
 *        so those round instead. Caps are left square (the default), since a
 *        round one would lengthen every dash `with-dash` draws.
 * @returns how far inside its box the shape's path lies: half the line width,
 *          since the stroke is centred on the path and the box was grown by
 *          the whole of it (#431). Zero for a solid shape.
 */
function beginShape (
  ctx: CanvasRenderingContext2D, drawing: Strokeable, join: CanvasLineJoin = 'round'
): number {
  ctx.fillStyle = color_rgbToString(drawing.color)
  ctx.strokeStyle = color_rgbToString(drawing.color)
  const lineWidth = drawing.lineWidth ?? 0
  if (drawing.mode === 'outline') {
    ctx.lineWidth = lineWidth
    ctx.lineJoin = join
  }
  return lineWidth / 2
}

/**
 * Paints the current path as the shape's fill mode calls for.
 *
 * N.B., a mode that is neither 'solid' nor 'outline' used to fall through every
 * branch and draw *nothing*, silently -- which is what (ellipse w h #t color)
 * did, the very call ellipse's own (wrong) `boolean?` contract demanded. The
 * fill-mode? contract now stops that at construction; this is the backstop.
 */
function fillOrStroke (ctx: CanvasRenderingContext2D, drawing: Strokeable): void {
  if (drawing.mode === 'solid') {
    ctx.fill()
  } else {
    ctx.stroke()
  }
}

export function drawing_render (x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  const ctx = context2d(canvas)
  // The canvas may be the user's own (`canvas-drawing!`), so the stroke
  // settings a shape needs are put back before returning rather than left
  // behind for whatever they draw next.
  ctx.save()
  try {
    renderShape(x, y, drawing, canvas, ctx)
  } finally {
    ctx.restore()
  }
}

function renderShape (
  x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement,
  ctx: CanvasRenderingContext2D
) {
  switch (drawing[L.structKind]) {
    case 'ellipse': {
      const inset = beginShape(ctx, drawing)
      const radiusX = (drawing.width - 2 * inset) / 2
      const radiusY = (drawing.height - 2 * inset) / 2
      ctx.beginPath()
      ctx.ellipse(x + drawing.width / 2, y + drawing.height / 2, radiusX, radiusY, 0, 0, 2 * Math.PI)
      fillOrStroke(ctx, drawing)
      break
    }
    case 'rectangle': {
      const inset = beginShape(ctx, drawing, 'miter')
      if (drawing.mode === 'solid') {
        ctx.fillRect(x, y, drawing.width, drawing.height)
      } else {
        ctx.strokeRect(x + inset, y + inset,
          drawing.width - 2 * inset, drawing.height - 2 * inset)
      }
      break
    }
    case 'triangle': {
      const inset = beginShape(ctx, drawing)
      const left = x + inset
      const right = x + drawing.width - inset
      const top = y + inset
      const bottom = y + drawing.height - inset
      ctx.beginPath()
      // Start in the bottom-left corner of the triangle...
      ctx.moveTo(left, bottom)
      // Then go to the top corner...
      ctx.lineTo((left + right) / 2, top)
      // And then the bottom-right corner...
      ctx.lineTo(right, bottom)
      // And back!
      ctx.lineTo(left, bottom)
      fillOrStroke(ctx, drawing)
      break
    }
    case 'path': {
      if (drawing.points.length === 0) { break }
      const inset = beginShape(ctx, drawing)
      ctx.beginPath()
      ctx.moveTo(x + inset + drawing.points[0][0], y + inset + drawing.points[0][1])
      drawing.points.slice(1).forEach(p => {
        ctx.lineTo(x + inset + p[0], y + inset + p[1])
      })
      fillOrStroke(ctx, drawing)
      break
    }
    case 'beside': {
      drawing.drawings.forEach(d => {
        drawing_render(
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
        drawing_render(
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
        drawing_render(
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
      drawing_render(x2, y2, drawing.d2, canvas)
      drawing_render(x1, y1, drawing.d1, canvas)
      break
    }
    case 'rotate': {
      const offsetX = x + drawing.dx
      const offsetY = y + drawing.dy
      const angle = drawing.angle * Math.PI / 180
      // N.B., need to move the canvas from the origin to the
      // center of the drawing to rotate and then move back to
      // the origin.
      ctx.translate(offsetX, offsetY)
      ctx.rotate(angle)
      
      drawing_render(0, 0, drawing.drawing, canvas)
      
      ctx.rotate(-angle)
      ctx.translate(-offsetX, -offsetY)
      break
    }
    case 'withDash': {
      ctx.setLineDash(drawing.dashSpec)
      drawing_render(x, y, drawing.drawing, canvas)
      ctx.setLineDash([])
      break
    }
    case 'text': {
      ctx.fillStyle = color_rgbToString(drawing.color)
      ctx.font = font_fontToFontString(drawing.font, drawing.size) 
      const metrics = ctx.measureText(drawing.text)
      ctx.fillText(drawing.text, x, y + metrics.actualBoundingBoxAscent + 1)
    }
  }
}

/**
 * Clears `canvas` to a solid background before a drawing is rendered onto it.
 * @param background the fill color (default 'white'). Callers rendering for
 *   *display* pass a themed color (see DrawingRenderer.vue); the default keeps
 *   off-screen/data uses (drawing->pixels, drawing->image) deterministic.
 */
export function drawing_clearDrawing (canvas: HTMLCanvasElement, background = 'white') {
  const ctx = context2d(canvas)
  ctx.fillStyle = background
  ctx.strokeStyle = 'black'
  ctx.fillRect(0, 0, Math.ceil(canvas.width), Math.ceil(canvas.height))
}

// TODO: aria labels should be in a central location
export const drawing_canvasAriaLabel = 'scamper-canvas'
export function drawing_renderer (drawing: Drawing): HTMLElement {
  const canvas = document.createElement('canvas')
  canvas.setAttribute('aria-label', drawing_canvasAriaLabel)
  canvas.width = Math.ceil(drawing.width)
  canvas.height = Math.ceil(drawing.height)
  drawing_clearDrawing(canvas)
  drawing_render(0, 0, drawing, canvas)
  return canvas
}