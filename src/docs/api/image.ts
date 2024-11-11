import { ArgDoc, Doc, NoArgDoc } from './docs.js'

export const imageQ: Doc = new Doc(
  'image?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is an image.'
)

export const shapeQ: Doc = new Doc(
  'shape?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only `v` is a shape.'
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

export const colorQ: Doc = new Doc(
  'color?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only if `v` is a valid color: a string containing a named color, an `rgb` value, or an `hsv` value.'
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

export const solidEllipse: Doc = new Doc(
  'solid-ellipse',
  'drawing?',
  [
    new ArgDoc('width', 'integer?'),
    new ArgDoc('height', 'integer?'),
    new ArgDoc('color', 'string?')
  ],
  'Returns a new drawing containing a solid ellipse with dimensions `width × height`.'
)

export const outlinedEllipse: Doc = new Doc(
  'outlined-ellipse',
  'drawing?',
  [
    new ArgDoc('width', 'integer?'),
    new ArgDoc('height', 'integer?'),
    new ArgDoc('color', 'string?')
  ],
  'Returns a new drawing containing an outlined ellipse with dimensions `width × height`.'
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

export const solidCircle: Doc = new Doc(
  'solid-circle',
  'drawing?',
  [
    new ArgDoc('radius', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a solid circle of radius `radius`.'
)

export const outlinedCircle: Doc = new Doc(
  'outlined-circle',
  'drawing?',
  [
    new ArgDoc('radius', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of an outlined circle of radius `radius`.'
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

export const solidRectangle: Doc = new Doc(
  'solid-rectangle',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('height', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a solid rectangle with dimensions `width × height`.'
)

export const outlineRectangle: Doc = new Doc(
  'outlined-rectangle',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('height', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of an outlined rectangle with dimensions `width × height`.'
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

export const solidSquare: Doc = new Doc(
  'solid-square',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a solid square with length `width`.'
)

export const outlineSquare: Doc = new Doc(
  'outlined-square',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of an outline square with length `width`.'
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

export const solidTriangle: Doc = new Doc(
  'solid-triangle',
  'drawing?',
  [
    new ArgDoc('length', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a solid equilateral triangle with length `length`.'
)

export const outlinedTriangle: Doc = new Doc(
  'outlined-triangle',
  'drawing?',
  [
    new ArgDoc('length', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of an outlined equilateral triangle with length `length`.'
)

export const isoscelesTriangle: Doc = new Doc(
  'isosceles-triangle',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('height', 'number?'),
    new ArgDoc('fill', 'string?, either "solid" or "outline"'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a isosceles triangle with base `base` and height `height`.'
)

export const solidIsoscelesTriangle: Doc = new Doc(
  'solid-isosceles-triangle',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('height', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of a solid isosceles triangle with base `base` and height `height`.'
)

export const outlinedIsoscelesTriangle: Doc = new Doc(
  'outlined-isosceles-triangle',
  'drawing?',
  [
    new ArgDoc('width', 'number?'),
    new ArgDoc('height', 'number?'),
    new ArgDoc('color', 'string?, either a color name or the form "rgba(r, g, b, a)"')
  ],
  'Returns a drawing consisting of an outlined isosceles triangle with base `base` and height `height`.'
)

export const imageWidth: Doc = new Doc(
  'image-width',
  'number?',
  [ new ArgDoc('img', 'drawing?') ],
  'Returns the width of the image.'
)

export const imageHeight: Doc = new Doc(
  'image-height',
  'number?',
  [ new ArgDoc('img', 'drawing?') ],
  'Returns the height of the image.'
)

export const imageColor: Doc = new Doc(
  'image-color',
  'string?',
  [ new ArgDoc('img', 'drawing?') ],
  'Returns the color of the image.'
)

export const imageRecolor: Doc = new Doc(
  'image-recolor',
  'drawing?',
  [
    new ArgDoc('img', 'drawing?'),
    new ArgDoc('color', 'color?')
  ],
  'Returns a new image with the same dimensions as `img` but with the color `color`.'
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

export const withDash: Doc = new Doc(
  'with-dash',
  'drawing?',
  [
    new ArgDoc('dash-spec', 'list?, a list of numbers'),
    new ArgDoc('d', 'drawing?')
  ],
  'Returns a new drawing formed by drawing `d` but with lines drawn according to `dash-spec`. `dash-spec` is an list of numbers where each successive pair of numbers describe the length of a dash and the length of the subsequent gap.'
)

export const font: Doc = new Doc(
  'font',
  'font?',
  [
    new ArgDoc('face', 'string? A valid font name'),
    new ArgDoc('system-face', 'string? A generic font family name (optional, default "sans-serif")'),
    new ArgDoc('bold?', 'boolean? (optional, default #f)'),
    new ArgDoc('italic?', 'boolean? (optional, default #f)'),
  ],
  'Returns a new font value with the given arguments. The `system-face` name is drawn from one of the possible system font families, a list can be found on [MDN (font-family)](https://developer.mozilla.org/en-US/docs/Web/CSS/font-family#syntax)'
)

export const text: Doc = new Doc(
  'text',
  'drawing?',
  [
    new ArgDoc('str', 'string?'),
    new ArgDoc('size', 'number? A valid font size (in px)'),
    new ArgDoc('color', 'color?'),
    new ArgDoc('font', 'font? (optional, default (font "Arial"))')
  ],
  'Returns a new drawing formed by drawing `str` with the given arguments.'
)

export const isRgbComponent: Doc = new Doc(
  'rgb-component?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only if `v` is an integer between 0 and 255.'
)

export const isRgb: Doc = new Doc(
  'rgb?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only if `v` is a rgb value.'
)

export const rgb: Doc = new Doc(
  'rgb',
  'rgb?',
  [
    new ArgDoc('r', 'rgb-component?'),
    new ArgDoc('g', 'rgb-component?'),
    new ArgDoc('b', 'rgb-component?'),
    new ArgDoc('a', 'rgb-component?, optional')
  ],
  'Returns an rgb value with the specified components.'
)

export const rbgRed: Doc = new Doc(
  'rgb-red',
  'rgbComponent?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns the red component of the rgb value.'
)

export const rbgGreen: Doc = new Doc(
  'rgb-green',
  'rgbComponent?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns the green component of the rgb value.'
)

export const rbgBlue: Doc = new Doc(
  'rgb-blue',
  'rgbComponent?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns the blue component of the rgb value.'
)

export const rbgAlpha: Doc = new Doc(
  'rgb-alpha',
  'rgbComponent?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns the alpha component of the rgb value.'
)

export const rgbDistance: Doc = new Doc(
  'rgb-distance',
  'number?',
  [
    new ArgDoc('rgb1', 'rgb?'),
    new ArgDoc('rgb2', 'rgb?')
  ],
  'Returns the Euclidean distance between the two rgb values.'
)

export const isColorName: Doc = new Doc(
  'color-name?',
  'boolean?',
  [ new ArgDoc('v', 'string') ],
  'Returns `#t` if and only if `v` is a valid color name.'
)

export const allColorNames: Doc = new Doc(
  'all-color-names',
  'list?',
  [ new NoArgDoc() ],
  'Returns a list of all valid color names.'
)

export const findColors: Doc = new Doc(
  'find-colors',
  'list?',
  [ new ArgDoc('color-name', 'string') ],
  'Returns a list of all color names that contain `color`, case-insensitive.'
)

export const rbgToString: Doc = new Doc(
  'rgb->string',
  'string?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a string representation of the rgb value, e.g., approrpiate for use as a shape color.'
)

export const isHsv: Doc = new Doc(
  'hsv?',
  'boolean?',
  [ new ArgDoc('v', 'any') ],
  'Returns `#t` if and only if `v` is a hsv value.'
)

export const hsv: Doc = new Doc(
  'hsv',
  'hsv?',
  [
    new ArgDoc('h', 'number?, 0 <= h <= 360'),
    new ArgDoc('s', 'number?, 0 <= s <= 100'),
    new ArgDoc('v', 'number?, 0 <= v <= 100'),
    new ArgDoc('a', 'number?, 0 <= a <= 255')
  ],
  'Returns a hsv value with the specified components.'
)

export const hsvHue: Doc = new Doc(
  'hsv-hue',
  'number?',
  [ new ArgDoc('hsv', 'hsv?') ],
  'Returns the hue component of the hsv value.'
)

export const hsvSaturation: Doc = new Doc(
  'hsv-saturation',
  'number?',
  [ new ArgDoc('hsv', 'hsv?') ],
  'Returns the saturation component of the hsv value.'
)

export const hsvValue: Doc = new Doc(
  'hsv-value',
  'number?',
  [ new ArgDoc('hsv', 'hsv?') ],
  'Returns the value component of the hsv value.'
)

export const hsvAlpha: Doc = new Doc(
  'hsv-alpha',
  'number?',
  [ new ArgDoc('hsv', 'hsv?') ],
  'Returns the alpha component of the hsv value.'
)

export const hsvComplement: Doc = new Doc(
  'hsv-complement',
  'hsv?',
  [ new ArgDoc('hsv', 'hsv?') ],
  'Returns the complement of the hsv value.'
)

export const hsvToRgb: Doc = new Doc(
  'hsv->rgb',
  'rgb?',
  [ new ArgDoc('hsv', 'hsv?') ],
  'Converts the hsv value to an rgb value.'
)

export const rgbHue: Doc = new Doc(
  'rgb-hue',
  'number?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns the hue component of the rgb value.'
)

export const rgbSaturation: Doc = new Doc(
  'rgb-saturation',
  'number?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns the saturation component of the rgb value.'
)

export const rgbValue: Doc = new Doc(
  'rgb-value',
  'number?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns the value component of the rgb value.'
)

export const rgbToHsv: Doc = new Doc(
  'rgb->hsv',
  'hsv?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Converts the rgb value to an hsv value.'
)

export const hsvToString: Doc = new Doc(
  'hsv->string',
  'string?',
  [ new ArgDoc('hsv', 'hsv?') ],
  'Returns a string representation of the hsv value.'
)

export const colorNameToRgb: Doc = new Doc(
  'color-name->rgb',
  'rgb?',
  [ new ArgDoc('color-name', 'string') ],
  'Returns the rgb value of the color name.'
)

export const colorNameToHsv: Doc = new Doc(
  'color-name->hsv',
  'hsv?',
  [ new ArgDoc('color-name', 'string') ],
  'Returns the hsv value of the color name.'
)

export const rgbDarker: Doc = new Doc(
  'rgb-darker',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a darker version of the rgb value.'
)

export const rgbLighter: Doc = new Doc(
  'rgb-lighter',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a lighter version of the rgb value.'
)

export const rgbRedder: Doc = new Doc(
  'rgb-redder',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a redder version of the rgb value.'
)

export const rgbGreener: Doc = new Doc(
  'rgb-greener',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a greener version of the rgb value.'
)

export const rgbBluer: Doc = new Doc(
  'rgb-bluer',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a bluer version of the rgb value.'
)

export const rgbPseudoComplement: Doc = new Doc(
  'rgb-pseudo-complement',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a pseudo-complement of the rgb value.'
)

export const rgbGreyscale: Doc = new Doc(
  'rgb-greyscale',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a greyscale version of the rgb value.'
)

export const rgbPhaseshift: Doc = new Doc(
  'rgb-phaseshift',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a phaseshifted version of the rgb value.'
)

export const rgbRotateComponents: Doc = new Doc(
  'rgb-rotate-components',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a rotated version of the rgb value.'
)

export const rgbThin: Doc = new Doc(
  'rgb-thin',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a thinner version of the rgb value.'
)

export const rgbThicken: Doc = new Doc(
  'rgb-thicken',
  'rgb?',
  [ new ArgDoc('rgb', 'rgb?') ],
  'Returns a thicker version of the rgb value.'
)

export const rgbAdd: Doc = new Doc(
  'rgb-add',
  'rgb?',
  [
    new ArgDoc('rgb1', 'rgb?'),
    new ArgDoc('rgb2', 'rgb?')
  ],
  'Returns the sum of the two rgb values.'
)

export const rgbSubtract: Doc = new Doc(
  'rgb-subtract',
  'rgb?',
  [
    new ArgDoc('rgb1', 'rgb?'),
    new ArgDoc('rgb2', 'rgb?')
  ],
  'Returns the difference of the two rgb values.'
)

export const rgbAverage: Doc = new Doc(
  'rgb-average',
  'rgb?',
  [
    new ArgDoc('rgb1', 'rgb?'),
    new ArgDoc('rgb2', 'rgb?')
  ],
  'Returns the average of the two rgb values.'
)

export const withImageFile: Doc = new Doc(
  'with-image-file',
  'html?',
  [ new ArgDoc('callback', 'function?') ],
  'Returns a container with a file chooser that, when used, calls `callback` with the selected image and replaces the container\'s contentsr with the output produced by `callback`.'
)

export const withImageFromUrl: Doc = new Doc(
  'with-image-from-url',
  'html?',
  [
    new ArgDoc('url', 'string?'),
    new ArgDoc('callback', 'function?')
  ],
  'Returns a container that, when used, calls `callback` with the selected image and replaces the chooser with the output produced by `callback`.'
)

export const pixelMap: Doc = new Doc(
  'pixel-map',
  'image?',
  [
    new ArgDoc('fn', 'function?'),
    new ArgDoc('img', 'image?')
  ],
  'Returns a new `img` that is the result of applying `fn` to each pixel (represented as a rgb value) of the original `img`.'
)

export const imageGetPixel: Doc = new Doc(
  'image-get-pixel',
  'rgb?',
  [
    new ArgDoc('img', 'image?'),
    new ArgDoc('x', 'integer?'),
    new ArgDoc('y', 'integer?')
  ],
  'Returns the rgb value of the pixel at position `(x, y)` in the image.'
)
