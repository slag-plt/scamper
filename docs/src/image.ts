import Doc from './docs.js'

export const image: Doc = new Doc(
  '(image? v): boolean?', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is an image.'
)

export const color: Doc = new Doc(
  '(color r b g a): string?', [
    'r: integer?, 0 <= r <= 255',
    'b: integer?, 0 <= b <= 255',
    'g: integer?, 0 <= g <= 255',
    'a: integer?, 0 <= a <= 1'
  ],
  'Returns a string of the form `"rgba(r, g, b, a)"` appropriate for use as a color.'
)

export const ellipse: Doc = new Doc(
  '(ellipse width height fill color): drawing?', [
    'width: integer?',
    'height: integer?',
    'fill: boolean?',
    'color: string?'
  ],
  'Returns a new drawing containing an ellipse with dimensions `width × height`.'
)

export const circle: Doc = new Doc(
  '(circle radius fill color): drawing?', [
    'radius: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a circle of radius `radius`.'
)

export const rectangle: Doc = new Doc(
  '(rectangle width height fill color): drawing?', [
    'width: number?',
    'height: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a rectangle with dimensions `width × height`.'
)

export const drawingSquare: Doc = new Doc(
  '(square width fill color): drawing?', [
    'width: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a square with length `width`.'
)

export const triangle: Doc = new Doc(
  '(triangle length fill color): drawing?', [
    'length: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a equilateral triangle with length `length`.'
)

export const path: Doc = new Doc(
  '(path width height points fill color): drawing?', [
    'width: number?',
    'height: number?',
    'points: list?, a list of points, pairs of numbers',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing with dimensions `width × height` formed by connecting the points in `points` with straight lines. The points are specified as a `pair` of coordinates.'
)

export const beside: Doc = new Doc(
  '(beside d1 d2 ...): drawing?', [
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other (horizontally).'
)

export const besideAlign: Doc = new Doc(
  '(beside/align align d1 d2 ...): drawing?', [
    'align: string?, either "top", "center", or "bottom"',
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other on the x-axis, aligning them along the y-axis according to `align`.'
)

export const above: Doc = new Doc(
  '(above d1 d2 ...): drawing?', [
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other (vertically in descending order).'
)

export const aboveAlign: Doc = new Doc(
  '(above/align align d1 d2 ...): drawing?', [
    'align: string?, either "left", "middle", or "right"',
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other on the y-axis, aligning them along the x-axis according to `align`.'
)

export const overlay: Doc = new Doc(
  '(overlay d1 d2 ...): drawing?', [
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other. (`d1` is the topmost drawing).'
)

export const overlayAlign: Doc = new Doc(
  '(overlay/align xAlign yAlign d1 d2 ...): drawing?', [
    'xAlign: string?, either "left", "middle", or "right"',
    'yAlign: string?, either "top", "center", or "bottom"',
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other, aligning them according to `xAlign` and `yAlign`.'
)

export const overlayOffset: Doc = new Doc(
  '(overlay/offset d1 dx dy d2): drawing?', [
    'd1: drawing?',
    'dx: number?',
    'dy: number?',
    'd2: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1` on top of `d2`, offset by `(dx, dy)`.'
)

export const rotate: Doc = new Doc(
  '(rotate angle d): drawing?', [
    'angle: number?, in degrees',
    'd: drawing?'
  ],
  'Returns a new drawing formed by rotating drawing `d` by `angle` degrees around the center of its bounding box. Note: currently buggy and shifts off-center.'
)

export const withDashes: Doc = new Doc(
  '(with-dashes dash-spec d): drawing?', [
    'dash-spec: list?, a list of numbers',
    'd: drawing?'
  ],
  'Returns a new drawing formed by drawing `d` but with dashes specified by `dash-spec`. `dash-spec` is an list of numbers where each successive pair of numbers describe the length of a dash and the length of the subsequent gap.'
)
