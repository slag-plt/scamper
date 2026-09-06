import * as L from '../../lpm'
import { Rgb, color_rgb, color_colorToRgb, color_rgbAverage, color_rgbToString, color_describeColor } from './color.js'
import { Font, font_font, font_fontToFontString } from './font.js'
import { context2d } from './context.js'
import { requireBrowser } from '../browser.js'

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
 * half of it falls outside (#431); the stroke itself, present only when there
 * is one; and the student's own description, likewise present only when given
 * (see `drawing_drawingDescription`).
 */
function shapeFields(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string) {
  const stroke = strokeWidth(mode, lineWidth)
  return {
    width: width + (stroke ?? 0),
    height: height + (stroke ?? 0),
    mode,
    color: color_colorToRgb(color),
    ...(stroke !== undefined ? { lineWidth: stroke } : {}),
    ...(description !== undefined ? { description } : {})
  }
}

/**
 * The description a student attached to a drawing, if any. Every drawing may
 * carry one; `drawing_drawingDescription` falls back to generating one from the
 * structure when it does not.
 */
interface Described {
  description?: string
}

export type Drawing = Ellipse | Rectangle | Triangle | Path | Wedge | Beside | Above | Overlay | OverlayOffset | Rotate | WithDash | DText

/** A fill mode: the string "solid" or "outline". */
export function drawing_fillModeQ (v: L.Value): boolean {
  return v === 'solid' || v === 'outline'
}

export function drawing_drawingQ (v: L.Value): boolean {
  return L.isStructKind(v, 'ellipse') || L.isStructKind(v, 'rectangle') ||
         L.isStructKind(v, 'triangle') || L.isStructKind(v, 'path') ||
         L.isStructKind(v, 'wedge') ||
         L.isStructKind(v, 'beside') || L.isStructKind(v, 'above') ||
         L.isStructKind(v, 'overlay') || L.isStructKind(v, 'overlayOffset') ||
         L.isStructKind(v, 'rotate') || L.isStructKind(v, 'withDash') ||
         L.isStructKind(v, 'text')
}
// TODO: in the new 151 library, images generalize to more than just shapes!
// In particular, images include shapes, image files, etc. We don't have
// such a unified view in Scamper (yet), so for now, shape? is an alias of
// image?

interface Ellipse extends L.Struct, Described {
  [L.structKind]: 'ellipse',
  width: number,
  height: number,
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

const ellipsePrim = (width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Ellipse => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'ellipse',
  ...shapeFields(width, height, mode, color, lineWidth, description)
})

export function drawing_ellipse(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Ellipse {
  return ellipsePrim(width, height, mode, color, lineWidth, description)
}

/**
 * A circle is sized by its diameter, not its radius, so that the one number it
 * takes means what `square`'s does: `(circle d ...)` is `d` across (#433).
 */
export function drawing_circle(diameter: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Ellipse {
  return ellipsePrim(diameter, diameter, mode, color, lineWidth, description)
}

interface Rectangle extends L.Struct, Described {
  [L.structKind]: 'rectangle',
  width: number,
  height: number,
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

const rectanglePrim = (width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Rectangle => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'rectangle',
  ...shapeFields(width, height, mode, color, lineWidth, description)
})

export function drawing_rectangle(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Rectangle {
  return rectanglePrim(width, height, mode, color, lineWidth, description)
}

export function drawing_square(length: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Rectangle {
  return rectanglePrim(length, length, mode, color, lineWidth, description)
}

interface Triangle extends L.Struct, Described {
  [L.structKind]: 'triangle',
  width: number,
  height: number,
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

const trianglePrim = (width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Triangle => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'triangle',
  ...shapeFields(width, height, mode, color, lineWidth, description)
})

/**
 * Scamper's `triangle` is the *equilateral* one -- the height it derives is what
 * makes all three edges `length` -- so `drawing_equilateralTriangle` below is an
 * alias rather than a separate shape (#432).
 */
export function drawing_triangle(length: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Triangle {
  return trianglePrim(length, length * Math.sqrt(3) / 2, mode, color, lineWidth, description)
}

export function drawing_isoscelesTriangle(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Triangle {
  return trianglePrim(width, height, mode, color, lineWidth, description)
}

interface Path extends L.Struct, Described {
  [L.structKind]: 'path',
  width: number,
  height: number,
  points: [number, number][],
  /**
   * Whether the last vertex joins back to the first. A polygon is closed, so an
   * outlined one is stroked all the way round; `path` is an open polyline and
   * stays that way, which is the whole difference between the two. Carried only
   * when it is true, as `lineWidth` and `description` are, so an open path
   * prints exactly as it did before there was anything to distinguish.
   */
  closed?: boolean,
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

// N.B., `points` sits between the box and the mode, matching the argument
// order of `path` itself, so the fields shapeFields supplies are spread on
// either side of it rather than all at the end.
const pathPrim = (width: number, height: number, points: [number, number][], closed: boolean, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Path => {
  const { width: w, height: h, ...rest } = shapeFields(width, height, mode, color, lineWidth, description)
  return {
    [L.scamperTag]: 'struct', [L.structKind]: 'path',
    width: w, height: h, points, ...(closed ? { closed } : {}), ...rest
  }
}

/** The `(x . y)` pairs of `points` as the coordinate array a Path stores. */
function pathPoints(points: L.List): [number, number][] {
  return L.listToVector(points).map(
    (p: L.Value) => [(p as L.Pair).fst, (p as L.Pair).snd]
  ) as [number, number][]
}

export function drawing_path(width: number, height: number, points: L.List, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Path {
  return pathPrim(width, height, pathPoints(points), false, mode, color, lineWidth, description)
}

/***** Polygons ***************************************************************/

// A polygon, a diamond, and a right triangle are all a Path: they differ only
// in the vertices they compute, so they need no rendering of their own. The
// predicates below recover which is which from those vertices, in the spirit of
// csc151, whose `rectangle?` likewise accepts a polygon shaped like one (#432).
//
// N.B., ours does not go that far: `rectangle?` here is the Rectangle struct
// alone, so a four-sided polygon is not a rectangle. Going further would mean
// teaching `rectangle-width` to read a polygon's vertices as well, and nothing
// needs it yet.

/** The bounding box of `points` as `[xMin, yMin, width, height]`. */
function pointsBounds(points: [number, number][]): [number, number, number, number] {
  if (points.length === 0) { return [0, 0, 0, 0] }
  const xs = points.map(([x, _]) => x)
  const ys = points.map(([_, y]) => y)
  const xMin = Math.min(...xs)
  const yMin = Math.min(...ys)
  return [xMin, yMin, Math.max(...xs) - xMin, Math.max(...ys) - yMin]
}

/**
 * A polygon with the given vertices. Unlike `path`, it is not told a size:
 * the points are translated to their own bounding box, so a student names
 * vertices in whatever coordinates suit them and the shape is sized to fit.
 */
export function drawing_polygon(points: L.List, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Path {
  const pts = pathPoints(points)
  const [xMin, yMin, width, height] = pointsBounds(pts)
  return pathPrim(width, height, pts.map(([x, y]) => [x - xMin, y - yMin]),
    true, mode, color, lineWidth, description)
}

/** The vertices of a `width` x `height` diamond, per csc151's `diamond-points`. */
function diamondPoints(width: number, height: number): [number, number][] {
  return [[width / 2, 0], [width, height / 2], [width / 2, height], [0, height / 2]]
}

export function drawing_diamond(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Path {
  return pathPrim(width, height, diamondPoints(width, height), true, mode, color, lineWidth, description)
}

/**
 * The vertices of a `width` x `height` right triangle, per csc151's
 * `right-triangle-points`: the right angle is at the bottom-left.
 */
function rightTrianglePoints(width: number, height: number): [number, number][] {
  return [[0, 0], [width, height], [0, height]]
}

export function drawing_rightTriangle(width: number, height: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Path {
  return pathPrim(width, height, rightTrianglePoints(width, height), true, mode, color, lineWidth, description)
}

/** An equilateral triangle; see `drawing_triangle`, which already is one. */
export function drawing_equilateralTriangle(edge: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Triangle {
  return drawing_triangle(edge, mode, color, lineWidth, description)
}

/***** Wedges *****************************************************************/

interface Wedge extends L.Struct, Described {
  [L.structKind]: 'wedge',
  width: number,
  height: number,
  radius: number,
  angle: number,
  mode: Mode,
  color: Rgb,
  lineWidth?: number
}

/**
 * The box a pie slice of radius `r` sweeping counterclockwise from 0 to
 * `angle` degrees occupies, and where its centre sits inside that box.
 *
 * The extremes of a slice are its centre, its two straight edges' far ends, and
 * whichever axis the arc crosses on the way round -- so those are the only
 * points that need measuring. Computed with y pointing up, then `cy` is flipped
 * to canvas coordinates on the way out.
 */
function wedgeBox(radius: number, angle: number): { width: number, height: number, cx: number, cy: number } {
  const sweep = Math.min(Math.abs(angle), 360)
  const toRadians = (degrees: number) => degrees * Math.PI / 180
  const xs = [0, radius, radius * Math.cos(toRadians(sweep))]
  const ys = [0, 0, radius * Math.sin(toRadians(sweep))]
  for (const crossing of [90, 180, 270]) {
    if (sweep >= crossing) {
      xs.push(radius * Math.cos(toRadians(crossing)))
      ys.push(radius * Math.sin(toRadians(crossing)))
    }
  }
  const xMin = Math.min(...xs)
  const yMax = Math.max(...ys)
  return {
    width: Math.max(...xs) - xMin,
    height: yMax - Math.min(...ys),
    cx: -xMin,
    cy: yMax
  }
}

const wedgePrim = (radius: number, angle: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Wedge => {
  // Checked here rather than left to the canvas, which raises a DOM
  // IndexSizeError on a negative radius -- not an error a student can catch.
  if (radius < 0) {
    throw new L.ScamperError('Runtime', `expected a non-negative radius, received ${radius.toString()}`)
  }
  const box = wedgeBox(radius, angle)
  const { width, height, ...rest } = shapeFields(box.width, box.height, mode, color, lineWidth, description)
  return {
    [L.scamperTag]: 'struct', [L.structKind]: 'wedge',
    width, height, radius, angle, ...rest
  }
}

/**
 * A wedge -- a slice of a circle of the given `radius`, sweeping `angle`
 * degrees counterclockwise from due east.
 */
export function drawing_wedge(radius: number, angle: number, mode: Mode, color: L.Value, lineWidth?: number, description?: string): Wedge {
  return wedgePrim(radius, angle, mode, color, lineWidth, description)
}

/**
 * The largest of `ns`, or 0 when there are none. `Math.max()` with no
 * arguments is `-Infinity`, which is the identity for max over the reals but
 * not over a drawing's non-negative dimensions: an empty composition would
 * take it as its width or height, and every enclosing `beside`/`above` that
 * *sums* that dimension would inherit it, silently shrinking the whole picture
 * to a 0x0 canvas (#517).
 */
const maxDim = (ns: number[]): number => ns.length === 0 ? 0 : Math.max(...ns)

interface Beside extends L.Struct, Described {
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
  height: maxDim(drawings.map(d => d.height)),
  drawings
})

export function drawing_beside(...drawings: Drawing[]): Beside {
  return besideAlignPrim('center', ...drawings)
}

export function drawing_besideAlign(align: string, ...drawings: Drawing[]): Beside {
  return besideAlignPrim(align, ...drawings)
}

interface Above extends L.Struct, Described {
  [L.structKind]: 'above',
  align: string,
  width: number,
  height: number,
  drawings: Drawing[]
}

const aboveAlignPrim = (align: string, ...drawings: Drawing[]): Above => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'above',
  align,
  width: maxDim(drawings.map(d => d.width)),
  height: drawings.reduce((acc, d) => acc + d.height, 0),
  drawings
})

export function drawing_above(...drawings: Drawing[]): Above {
  return aboveAlignPrim('middle', ...drawings)
}

export function drawing_aboveAlign(align: string, ...drawings: Drawing[]): Above {
  return aboveAlignPrim(align, ...drawings)
}

interface Overlay extends L.Struct, Described {
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
  width: maxDim(drawings.map(d => d.width)),
  height: maxDim(drawings.map(d => d.height)),
  drawings
})

export function drawing_overlay(...drawings: Drawing[]) {
  return overlayAlignPrim('middle', 'center', ...drawings)
}

export function drawing_overlayAlign(xAlign: string, yAlign: string, ...drawings: Drawing[]): Overlay {
  return overlayAlignPrim(xAlign, yAlign, ...drawings)
}

interface OverlayOffset extends L.Struct, Described {
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

interface Rotate extends L.Struct, Described {
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

interface WithDash extends L.Struct, Described {
  [L.structKind]: 'withDash',
  dashSpec: number[],
  drawing: Drawing,
  width: number,
  height: number
}

const withDashPrim = (dashSpec: number[], drawing: Drawing): WithDash => ({
  [L.scamperTag]: 'struct', [L.structKind]: 'withDash',
  dashSpec,
  drawing,
  width: drawing.width,
  height: drawing.height
})

/**
 * The dash lengths of `dashSpec` as the array a canvas takes (#491).
 * @throws ScamperError if a length is not a number, or is one setLineDash
 *         cannot use: it returns on a spec it cannot read rather than
 *         failing, so an unchecked one would quietly draw a solid line --
 *         a negative or non-finite length does that just as a string does.
 */
function checkDashSpec(dashSpec: L.List): number[] {
  return L.listToVector(dashSpec).map((v) => {
    if (typeof v !== 'number') {
      throw new L.ScamperError(
        'Runtime',
        `expected a list of numbers, but the list contains ${L.typeOf(v)}`
      )
    }
    if (!Number.isFinite(v) || v < 0) {
      throw new L.ScamperError(
        'Runtime',
        `expected a finite, non-negative dash length, received ${v.toString()}`
      )
    }
    return v
  })
}

export function drawing_withDash(dashSpec: L.List, drawing: Drawing): WithDash {
  return withDashPrim(checkDashSpec(dashSpec), drawing)
}
interface DText extends L.Struct, Described {
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

export function drawing_text(text: string, size: number, color: Rgb, font?: Font): DText {
  requireBrowser()
  const f: Font = font ?? font_font('Arial')

  // N.B., to calculate the width and height of text, we need to make a
  // temporary canvas to measure the text's dimensions.
  const canvas = document.createElement('canvas')
  const ctx = context2d(canvas)
  ctx.font = font_fontToFontString(f, size)
  const met = ctx.measureText(text)
  const width = met.width
  const height = met.actualBoundingBoxAscent + met.actualBoundingBoxDescent + 1

  return textPrim(width, height, text, f, size, color)
}

/***** Extended Functions *****************************************************/

export function drawing_solidSquare(length: number, color: L.Value, description?: string): Rectangle {
  return drawing_square(length, 'solid', color, undefined, description)
}

export function drawing_outlinedSquare(length: number, color: L.Value, lineWidth?: number, description?: string): Rectangle {
  return drawing_square(length, 'outline', color, lineWidth, description)
}

export function drawing_solidRectangle(width: number, height: number, color: L.Value, description?: string): Rectangle {
  return drawing_rectangle(width, height, 'solid', color, undefined, description)
}

export function drawing_outlinedRectangle(width: number, height: number, color: L.Value, lineWidth?: number, description?: string): Rectangle {
  return drawing_rectangle(width, height, 'outline', color, lineWidth, description)
}

export function drawing_solidCircle(diameter: number, color: L.Value, description?: string): Ellipse {
  return drawing_circle(diameter, 'solid', color, undefined, description)
}

export function drawing_outlinedCircle(diameter: number, color: L.Value, lineWidth?: number, description?: string): Ellipse {
  return drawing_circle(diameter, 'outline', color, lineWidth, description)
}

export function drawing_solidEllipse(width: number, height: number, color: L.Value, description?: string): Ellipse {
  return drawing_ellipse(width, height, 'solid', color, undefined, description)
}

export function drawing_outlinedEllipse(width: number, height: number, color: L.Value, lineWidth?: number, description?: string): Ellipse {
  return drawing_ellipse(width, height, 'outline', color, lineWidth, description)
}

export function drawing_solidTriangle(length: number, color: L.Value, description?: string): Triangle {
  return drawing_triangle(length, 'solid', color, undefined, description)
}

export function drawing_outlinedTriangle(length: number, color: L.Value, lineWidth?: number, description?: string): Triangle {
  return drawing_triangle(length, 'outline', color, lineWidth, description)
}

export function drawing_solidIsoscelesTriangle(width: number, height: number, color: L.Value, description?: string): Triangle {
  return drawing_isoscelesTriangle(width, height, 'solid', color, undefined, description)
}

export function drawing_outlinedIsoscelesTriangle(width: number, height: number, color: L.Value, lineWidth?: number, description?: string): Triangle {
  return drawing_isoscelesTriangle(width, height, 'outline', color, lineWidth, description)
}

export function drawing_solidEquilateralTriangle(edge: number, color: L.Value, description?: string): Triangle {
  return drawing_equilateralTriangle(edge, 'solid', color, undefined, description)
}

export function drawing_outlinedEquilateralTriangle(edge: number, color: L.Value, lineWidth?: number, description?: string): Triangle {
  return drawing_equilateralTriangle(edge, 'outline', color, lineWidth, description)
}

export function drawing_solidRightTriangle(width: number, height: number, color: L.Value, description?: string): Path {
  return drawing_rightTriangle(width, height, 'solid', color, undefined, description)
}

export function drawing_outlinedRightTriangle(width: number, height: number, color: L.Value, lineWidth?: number, description?: string): Path {
  return drawing_rightTriangle(width, height, 'outline', color, lineWidth, description)
}

export function drawing_solidPolygon(points: L.List, color: L.Value, description?: string): Path {
  return drawing_polygon(points, 'solid', color, undefined, description)
}

export function drawing_outlinedPolygon(points: L.List, color: L.Value, lineWidth?: number, description?: string): Path {
  return drawing_polygon(points, 'outline', color, lineWidth, description)
}

export function drawing_solidDiamond(width: number, height: number, color: L.Value, description?: string): Path {
  return drawing_diamond(width, height, 'solid', color, undefined, description)
}

export function drawing_outlinedDiamond(width: number, height: number, color: L.Value, lineWidth?: number, description?: string): Path {
  return drawing_diamond(width, height, 'outline', color, lineWidth, description)
}

export function drawing_solidWedge(radius: number, angle: number, color: L.Value, description?: string): Wedge {
  return drawing_wedge(radius, angle, 'solid', color, undefined, description)
}

export function drawing_outlinedWedge(radius: number, angle: number, color: L.Value, lineWidth?: number, description?: string): Wedge {
  return drawing_wedge(radius, angle, 'outline', color, lineWidth, description)
}

/***** Shape Queries **********************************************************/

/**
 * How close two lengths must be for a shape to count as the more specific one,
 * e.g. for `circle?` to accept an ellipse. csc151's tolerance, so that the same
 * shapes answer the same way in both libraries (#432).
 */
const shapeTolerance = 0.01

const approxEqual = (a: number, b: number): boolean =>
  Math.abs(a - b) <= shapeTolerance

/**
 * These predicates ask what a shape *is*, not how it was built -- a 20x20
 * ellipse is a circle, and a path whose vertices form a diamond is a diamond.
 * That is what csc151 does, and it is why `circle` and `square` need no
 * representation of their own here.
 */
export function drawing_ellipseQ(v: L.Value): boolean {
  return L.isStructKind(v, 'ellipse')
}

export function drawing_circleQ(v: L.Value): boolean {
  if (!L.isStructKind<Ellipse>(v, 'ellipse')) { return false }
  const [width, height] = shapeSize(v)
  return approxEqual(width, height)
}

export function drawing_rectangleQ(v: L.Value): boolean {
  return L.isStructKind(v, 'rectangle')
}

export function drawing_squareQ(v: L.Value): boolean {
  if (!L.isStructKind<Rectangle>(v, 'rectangle')) { return false }
  const [width, height] = shapeSize(v)
  return approxEqual(width, height)
}

export function drawing_isoscelesTriangleQ(v: L.Value): boolean {
  return L.isStructKind(v, 'triangle')
}

export function drawing_equilateralTriangleQ(v: L.Value): boolean {
  if (!L.isStructKind<Triangle>(v, 'triangle')) { return false }
  // On the inner size: a stroke grows both dimensions by the same amount, which
  // is not the ratio an equilateral triangle keeps between them.
  const [width, height] = shapeSize(v)
  return approxEqual(height, width * Math.sqrt(3) / 2)
}

export function drawing_polygonQ(v: L.Value): boolean {
  return L.isStructKind(v, 'path')
}

/** Whether `points` are `expected`, in order and within `shapeTolerance`. */
function pointsMatch(points: [number, number][], expected: [number, number][]): boolean {
  return points.length === expected.length &&
    points.every(([x, y], i) =>
      approxEqual(x, expected[i][0]) && approxEqual(y, expected[i][1]))
}

export function drawing_diamondQ(v: L.Value): boolean {
  if (!L.isStructKind<Path>(v, 'path')) { return false }
  const [, , width, height] = pointsBounds(v.points)
  return pointsMatch(v.points, diamondPoints(width, height))
}

export function drawing_rightTriangleQ(v: L.Value): boolean {
  if (!L.isStructKind<Path>(v, 'path')) { return false }
  const [, , width, height] = pointsBounds(v.points)
  return pointsMatch(v.points, rightTrianglePoints(width, height))
}

export function drawing_wedgeQ(v: L.Value): boolean {
  return L.isStructKind(v, 'wedge')
}

/** Every shape that carries a fill mode, i.e. everything but a composition. */
function asShape(v: L.Value): { mode: Mode } | undefined {
  return drawing_ellipseQ(v) || drawing_rectangleQ(v) ||
         drawing_isoscelesTriangleQ(v) || drawing_polygonQ(v) ||
         drawing_wedgeQ(v)
    ? v as unknown as { mode: Mode }
    : undefined
}

export function drawing_solidQ(v: L.Value): boolean {
  return asShape(v)?.mode === 'solid'
}

export function drawing_outlinedQ(v: L.Value): boolean {
  return asShape(v)?.mode === 'outline'
}

/***** Shape Accessors ********************************************************/

// All report the size the shape was *given*, not the box it occupies: an
// outlined shape's box is wider by its line width (#431), and asking a circle
// its diameter should return the number that built it.

export function drawing_ellipseWidth(v: Ellipse): number { return shapeSize(v)[0] }
export function drawing_ellipseHeight(v: Ellipse): number { return shapeSize(v)[1] }
export function drawing_circleDiameter(v: Ellipse): number { return shapeSize(v)[0] }

export function drawing_rectangleWidth(v: Rectangle): number { return shapeSize(v)[0] }
export function drawing_rectangleHeight(v: Rectangle): number { return shapeSize(v)[1] }
export function drawing_squareSide(v: Rectangle): number { return shapeSize(v)[0] }

export function drawing_isoscelesTriangleWidth(v: Triangle): number { return shapeSize(v)[0] }
export function drawing_isoscelesTriangleHeight(v: Triangle): number { return shapeSize(v)[1] }
export function drawing_equilateralTriangleEdge(v: Triangle): number { return shapeSize(v)[0] }

// From the vertices rather than the box, which a stroke has grown.
export function drawing_rightTriangleWidth(v: Path): number { return pointsBounds(v.points)[2] }
export function drawing_rightTriangleHeight(v: Path): number { return pointsBounds(v.points)[3] }
export function drawing_diamondWidth(v: Path): number { return pointsBounds(v.points)[2] }
export function drawing_diamondHeight(v: Path): number { return pointsBounds(v.points)[3] }

export function drawing_polygonPoints(v: Path): L.List {
  return L.vectorToList(v.points.map(([x, y]) => L.mkPair(x, y)))
}

export function drawing_wedgeRadius(v: Wedge): number { return v.radius }
export function drawing_wedgeAngle(v: Wedge): number { return v.angle }

/***** Descriptions ***********************************************************/

// A description is alt text: it is what `describe-image` returns and what the
// canvas announces to a screen reader. A student may give one to any shape, and
// one is generated from the structure otherwise -- so every image has some
// description, and a composition's is built from its parts' (#432).

/** How a description opens, article included: "a solid" or "an outlined". */
const modePhrase = (mode: Mode): string => mode === 'solid' ? 'a solid' : 'an outlined'

/** A length as a description should read it, rather than to full precision. */
const num = (n: number): string => `${Math.round(n * 100) / 100}`

/**
 * How far a generated description follows a composition: how deep it recurses,
 * and how many parts it lists at each level.
 *
 * Alt text is read aloud, so a description long enough to need these caps was
 * never usable anyway. They also keep the cost bounded: a description is
 * derived on demand rather than stored, and both renderers ask for one on every
 * repaint, so a drawing built by recursion -- a Sierpinski triangle, say, which
 * is an ordinary exercise -- would otherwise generate megabytes of it each time.
 */
const maxDescriptionDepth = 4
const maxDescribedParts = 6

/**
 * What `drawing` looks like, in words: the description it was given, or one
 * derived from its structure.
 *
 * Derived rather than stored, unlike csc151, which caches by mutating the
 * image. Nothing here is mutable, the caps above bound the work, and a
 * description that is never asked for costs nothing.
 */
export function drawing_drawingDescription(drawing: Drawing): string {
  return describeAt(drawing, 0)
}

/** `drawing` with `description` in place of whatever it carried. */
export function drawing_redescribe(drawing: Drawing, description: string): Drawing {
  return { ...drawing, description }
}

/**
 * `drawing` described `depth` levels below the one asked about.
 *
 * A description a student gave wins over both the derived wording and the depth
 * cut-off -- it is already short, and it is what they wanted said. Note that
 * this only reaches as far as the cut-off does: past `maxDescriptionDepth` a
 * composition is summarised, so a description below that point is not read.
 */
function describeAt(drawing: Drawing, depth: number): string {
  if (drawing.description !== undefined) { return drawing.description }
  if (depth >= maxDescriptionDepth) { return summarise(drawing) }
  return describeDrawing(drawing, depth)
}

/** A drawing too deeply nested to describe, named by what it is made of. */
function summarise(drawing: Drawing): string {
  switch (drawing[L.structKind]) {
    case 'beside':
    case 'above':
    case 'overlay':
      return `a composition of ${drawing.drawings.length} images`
    case 'overlayOffset':
      return 'a composition of 2 images'
    case 'rotate':
    case 'withDash':
      return 'an image'
    default:
      // A shape describes itself in one short phrase however deep it is.
      return describeDrawing(drawing, maxDescriptionDepth)
  }
}

/** The children's descriptions, joined by `separator`. */
function joinDescriptions(drawings: Drawing[], separator: string, depth: number): string {
  if (drawings.length === 0) { return 'no images' }
  const listed = drawings.slice(0, maxDescribedParts)
  const described = listed
    .map((d) => describeAt(d, depth + 1))
    .join(` ${separator} `)
  const remaining = drawings.length - listed.length
  return remaining === 0 ? described : `${described}, and ${remaining} more`
}

function describeDrawing(drawing: Drawing, depth: number): string {
  switch (drawing[L.structKind]) {
    case 'ellipse': {
      const [width, height] = shapeSize(drawing)
      const color = color_describeColor(drawing.color)
      return approxEqual(width, height)
        ? `${modePhrase(drawing.mode)} ${color} circle with diameter ${num(width)}`
        : `${modePhrase(drawing.mode)} ${color} ellipse with width ${num(width)} and height ${num(height)}`
    }
    case 'rectangle': {
      const [width, height] = shapeSize(drawing)
      const color = color_describeColor(drawing.color)
      return approxEqual(width, height)
        ? `${modePhrase(drawing.mode)} ${color} square with side ${num(width)}`
        : `${modePhrase(drawing.mode)} ${color} rectangle with width ${num(width)} and height ${num(height)}`
    }
    case 'triangle': {
      const [width, height] = shapeSize(drawing)
      const color = color_describeColor(drawing.color)
      return approxEqual(height, width * Math.sqrt(3) / 2)
        ? `${modePhrase(drawing.mode)} ${color} equilateral triangle with edge ${num(width)}`
        : `${modePhrase(drawing.mode)} ${color} isosceles triangle with width ${num(width)} and height ${num(height)}`
    }
    case 'path': {
      const mode = modePhrase(drawing.mode)
      const color = color_describeColor(drawing.color)
      const [, , width, height] = pointsBounds(drawing.points)
      const size = `with width ${num(width)} and height ${num(height)}`
      if (pointsMatch(drawing.points, diamondPoints(width, height))) {
        return `${mode} ${color} diamond ${size}`
      }
      if (pointsMatch(drawing.points, rightTrianglePoints(width, height))) {
        return `${mode} ${color} right triangle ${size}`
      }
      return `${mode} ${color} polygon with ${drawing.points.length} vertices`
    }
    case 'wedge':
      return `${modePhrase(drawing.mode)} ${color_describeColor(drawing.color)} wedge ` +
             `with radius ${num(drawing.radius)} and angle ${num(drawing.angle)}`
    case 'beside':
      return `a ${drawing.align}-aligned sequence of images ` +
             `(${joinDescriptions(drawing.drawings, 'beside', depth)})`
    case 'above':
      return `a ${drawing.align}-aligned stack of images ` +
             `(${joinDescriptions(drawing.drawings, 'above', depth)})`
    case 'overlay':
      return `overlaid images, aligned ${drawing.xAlign}-${drawing.yAlign} ` +
             `(${joinDescriptions(drawing.drawings, 'over', depth)})`
    case 'overlayOffset':
      return `one image (${describeAt(drawing.d1, depth + 1)}) offset by ` +
             `${num(drawing.dx)}, ${num(drawing.dy)} over another image ` +
             `(${describeAt(drawing.d2, depth + 1)})`
    case 'rotate':
      return `${describeAt(drawing.drawing, depth + 1)}, rotated by ${num(drawing.angle)} degrees`
    case 'withDash':
      return `${describeAt(drawing.drawing, depth + 1)}, drawn with dashed lines`
    case 'text':
      return `the text "${drawing.text}" in ${color_describeColor(drawing.color)}`
  }
}

// TODO: this need to be factored out to a general image lib that handles both
// drawings and canvases.

export function drawing_drawingWidth(drawing: Drawing): number {
  if (drawing_drawingQ(drawing)) {
    return drawing_normalize(drawing).width
  } else {
    return (drawing as unknown as HTMLCanvasElement).width
  }
}

export function drawing_drawingHeight(drawing: Drawing): number {
  if (drawing_drawingQ(drawing)) {
    return drawing_normalize(drawing).height
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
    case 'wedge':
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

/**
 * `drawing` in `color`, keeping whatever description it carried: a recolouring
 * is the same picture, so a description a student wrote for it still holds.
 * Applied here rather than threaded through each constructor below, so the
 * compound cases -- which rebuild from recoloured children -- get it too.
 */
export function drawing_drawingRecolor(drawing: Drawing, color: L.Value): Drawing {
  const recolored = recolorPrim(drawing, color)
  return drawing.description === undefined
    ? recolored
    : { ...recolored, description: drawing.description }
}

function recolorPrim(drawing: Drawing, color: L.Value): Drawing {
  switch(drawing[L.structKind]) {
    case 'ellipse':
      return ellipsePrim(...shapeSize(drawing), drawing.mode, color, drawing.lineWidth)
    case 'rectangle':
      return rectanglePrim(...shapeSize(drawing), drawing.mode, color, drawing.lineWidth)
    case 'triangle':
      return trianglePrim(...shapeSize(drawing), drawing.mode, color, drawing.lineWidth)
    case 'wedge':
      return wedgePrim(drawing.radius, drawing.angle, drawing.mode, color, drawing.lineWidth)
    case 'path': {
      const [width, height] = shapeSize(drawing)
      return pathPrim(width, height, drawing.points, drawing.closed ?? false, drawing.mode, color, drawing.lineWidth)
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
      return withDashPrim(drawing.dashSpec, drawing_drawingRecolor(drawing.drawing, color))
    case 'text':
      return textPrim(drawing.width, drawing.height, drawing.text,
        drawing.font, drawing.size, drawing.color)
  }
}

/**
 * Rewrites `drawing` so that a rotation of a rotation is the single equivalent
 * rotation, and so that every box above one is recomputed from the collapsed
 * subtree (#473).
 *
 * `rotate` boxes a drawing by turning the corners of the box it is handed, so
 * a second turn treats the first turn's padding as ink and the margin
 * compounds; `beside`/`above`/`overlay` snapshot their children's boxes at
 * construction, so the padding propagates upwards too. This is a *late* pass
 * over a finished structure -- construction is untouched, so the tree stays
 * exactly what the student wrote.
 *
 * Two invariants:
 *  - The painted ink is unchanged. Two rotations compose to the same net
 *    rotation under a pure translation, so collapsing moves no pixel.
 *  - A box only ever shrinks, never grows.
 *
 * Pure: nothing is mutated, so a subtree shared between drawings is safe, and
 * the original node is returned whenever nothing below it changed -- a drawing
 * with no nested rotation is handed straight back rather than rebuilt.
 */
export function drawing_normalize (drawing: Drawing): Drawing {
  const normalized = normalizePrim(drawing)
  // Rebuilding a collapsed subtree goes through the …Prim constructors, which
  // carry no description; the picture is unchanged, so its description is too.
  return normalized === drawing || drawing.description === undefined
    ? normalized
    : { ...normalized, description: drawing.description }
}

function normalizePrim (drawing: Drawing): Drawing {
  switch (drawing[L.structKind]) {
    case 'ellipse':
    case 'rectangle':
    case 'triangle':
    case 'path':
    case 'wedge':
    case 'text':
      return drawing
    case 'beside': {
      const drawings = drawing.drawings.map(drawing_normalize)
      return drawings.every((d, i) => d === drawing.drawings[i])
        ? drawing
        : besideAlignPrim(drawing.align, ...drawings)
    }
    case 'above': {
      const drawings = drawing.drawings.map(drawing_normalize)
      return drawings.every((d, i) => d === drawing.drawings[i])
        ? drawing
        : aboveAlignPrim(drawing.align, ...drawings)
    }
    case 'overlay': {
      const drawings = drawing.drawings.map(drawing_normalize)
      return drawings.every((d, i) => d === drawing.drawings[i])
        ? drawing
        : overlayAlignPrim(drawing.xAlign, drawing.yAlign, ...drawings)
    }
    case 'overlayOffset': {
      const d1 = drawing_normalize(drawing.d1)
      const d2 = drawing_normalize(drawing.d2)
      // The public constructor, not overlayOffsetPrim: the box must be
      // recomputed from the collapsed children, not carried over.
      return d1 === drawing.d1 && d2 === drawing.d2
        ? drawing
        : drawing_overlayOffset(drawing.dx, drawing.dy, d1, d2)
    }
    case 'rotate': {
      const child = drawing_normalize(drawing.drawing)
      // Not when the inner rotation carries a description of its own: collapsing
      // the two would leave nowhere to say it.
      if (child[L.structKind] === 'rotate' && child.description === undefined) {
        // The collapse. Summing modulo 360 is what makes (rotate 180 (rotate
        // 180 d)) exactly the original size rather than a rounding error away
        // from it; it only ever touches a sum this pass creates, so a single
        // rotation is left bit-identical.
        return drawing_rotate((drawing.angle + child.angle) % 360, child.drawing)
      }
      return child === drawing.drawing
        ? drawing
        : drawing_rotate(drawing.angle, child)
    }
    case 'withDash': {
      const child = drawing_normalize(drawing.drawing)
      return child === drawing.drawing
        ? drawing
        : withDashPrim(drawing.dashSpec, child)
    }
  }
}

export function drawing_drawingToPixels(drawing: Drawing): Rgb[] {
  requireBrowser()
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
  requireBrowser()
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
      // Without this a stroked polygon loses the edge back to its first vertex
      // -- fill() closes implicitly, but stroke() does not.
      if (drawing.closed) { ctx.closePath() }
      fillOrStroke(ctx, drawing)
      break
    }
    case 'wedge': {
      const inset = beginShape(ctx, drawing)
      const box = wedgeBox(drawing.radius, drawing.angle)
      const cx = x + inset + box.cx
      const cy = y + inset + box.cy
      const sweep = Math.min(Math.abs(drawing.angle), 360) * Math.PI / 180
      ctx.beginPath()
      ctx.moveTo(cx, cy)
      // Negated and drawn anticlockwise: a canvas measures its angles clockwise
      // because y points down, while a wedge sweeps counterclockwise from east.
      ctx.arc(cx, cy, drawing.radius, 0, -sweep, true)
      ctx.closePath()
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

/**
 * How a rendered drawing is found in the DOM. The aria-label used to serve
 * double duty as this handle; it now carries the drawing's description, which
 * is what a screen reader should read out, so the class is what identifies a
 * Scamper canvas (#432).
 */
export const drawing_canvasClass = 'scamper-canvas'

export function drawing_renderer (drawing: Drawing): HTMLElement {
  const d = drawing_normalize(drawing)
  const canvas = document.createElement('canvas')
  canvas.classList.add(drawing_canvasClass)
  canvas.setAttribute('aria-label', drawing_drawingDescription(d))
  canvas.width = Math.ceil(d.width)
  canvas.height = Math.ceil(d.height)
  drawing_clearDrawing(canvas)
  drawing_render(0, 0, d, canvas)
  return canvas
}