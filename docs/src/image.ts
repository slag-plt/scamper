import { ArgDoc, Doc } from './docs.js'

export const image: Doc = new Doc(
  'image?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is an image.'
)

export const color: Doc = new Doc(
  'color',
  'string?',
  [
    new ArgDoc('r', 'integer?, 0 <= r <= 255'),
    new ArgDoc('b', 'integer?, 0 <= b <= 255'),
    new ArgDoc('g', 'integer?, 0 <= g <= 255'),
    new ArgDoc('a', 'integer?, 0 <= a <= 1')
  ],
  'Returns a string of the form `"rgba(r, g, b, a)"` appropriate for use as a color.'
)

export const ellipse: Doc = new Doc(
  'ellipse',
  'drawing?',
  [
    new ArgDoc('width', 'integer?'),
    new ArgDoc('height', 'integer?'),
    new ArgDoc('fill', 'boolean?'),
    new ArgDoc('color', 'string?')
  ],
  'Returns a new drawing containing an ellipse with dimensions `width × height`.'
)

export const circle: Doc = new Doc(
  'circle',
  'drawing?',
  [
    new ArgDoc('radius', 'number?'),
    new ArgDoc('fill', 'string?, either "solid" or "outline"'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a circle of radius `radius`.'
)

export const rectangle: Doc = new Doc(
  'rectangle',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('height', 'number?'),
    new ArgDoc('fill', 'string?, either "solid" or "outline"'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a rectangle with dimensions `width × height`.'
)

export const drawingSquare: Doc = new Doc(
  'square',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('fill', 'string?, either "solid" or "outline"'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a square with length `width`.'
)

export const triangle: Doc = new Doc(
  'triangle',
  'drawing?',
  [
    new ArgDoc('length', 'number?'),
    new ArgDoc('fill', 'string?, either "solid" or "outline"'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a equilateral triangle with length `length`.'
)

export const path: Doc = new Doc(
  'path',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('height', 'number?'),
    new ArgDoc('points', 'list?, a list of points, pairs of numbers'),
    new ArgDoc('fill', 'string?, either "solid" or "outline"'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing with dimensions `width × height` formed by connecting the points in `points` with straight lines. The points are specified as a `pair` of coordinates.'
)

export const beside: Doc = new Doc(
  'beside',
  'drawing?',
  [ new ArgDoc('d1, d2, ...', 'drawing?') ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other (horizontally).'
)

export const besideAlign: Doc = new Doc(
  'beside/align',
  'drawing?',
  [
    new ArgDoc('align', 'string?, either "top", "center", or "bottom"'),
    new ArgDoc('d1, d2, ...', 'drawing?')
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other on the x-axis, aligning them along the y-axis according to `align`.'
)

export const above: Doc = new Doc(
  'above',
  'drawing?',
  [ new ArgDoc('d1, d2, ...', 'drawing?') ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other (vertically in descending order).'
)

export const aboveAlign: Doc = new Doc(
  'above/align',
  'drawing?',
  [
    new ArgDoc('align', 'string?, either "left", "middle", or "right"'),
    new ArgDoc('d1, d2, ...', 'drawing?')
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other on the y-axis, aligning them along the x-axis according to `align`.'
)

export const overlay: Doc = new Doc(
  'overlay',
  'drawing?',
  [ new ArgDoc('d1, d2, ...', 'drawing?') ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other. (`d1` is the topmost drawing).'
)

export const overlayAlign: Doc = new Doc(
  'overlay/align',
  'drawing?',
  [
    new ArgDoc('xAlign', 'string?, either "left", "middle", or "right"'),
    new ArgDoc('yAlign', 'string?, either "top", "center", or "bottom"'),
    new ArgDoc('d1, d2, ...', 'drawing?')
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other, aligning them according to `xAlign` and `yAlign`.'
)

export const overlayOffset: Doc = new Doc(
  'overlay/offset',
  'drawing?',
  [
    new ArgDoc('dx', 'number?'),
    new ArgDoc('dy', 'number?'),
    new ArgDoc('d1', 'drawing?'),
    new ArgDoc('d2', 'drawing?')
  ],
  'Creates a new drawing formed by places the drawing `d1` on top of `d2`, offset by `(dx, dy)`.'
)

export const rotate: Doc = new Doc(
  'rotate',
  'drawing?',
  [
    new ArgDoc('angle', 'number?, in degrees'),
    new ArgDoc('d', 'drawing?')
  ],
  'Returns a new drawing formed by rotating drawing `d` by `angle` degrees around the center of its bounding box. Note: currently buggy and shifts off-center.'
)

export const withDashes: Doc = new Doc(
  'with-dashes',
  'drawing?',
  [
    new ArgDoc('dash-spec', 'list?, a list of numbers'),
    new ArgDoc('d', 'drawing?')
  ],
  'Returns a new drawing formed by drawing `d` but with dashes specified by `dash-spec`. `dash-spec` is an list of numbers where each successive pair of numbers describe the length of a dash and the length of the subsequent gap.'
)
