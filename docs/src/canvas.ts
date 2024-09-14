import { ArgDoc, Doc } from './docs.js'

export const makeCanvasDoc: Doc = new Doc(
  'make-canvas',
  'canvas?',
  [
    new ArgDoc('width', 'integer?, positive'),
    new ArgDoc('height', 'integer?, positive')
  ],
  'Creates a canvas with the given width and height.'
)

export const drawRectangleDoc: Doc = new Doc(
  'draw-rectangle',
  'void?',
  [
    new ArgDoc('canvas', 'canvas?'),
    new ArgDoc('x', 'integer?'),
    new ArgDoc('y', 'integer?'),
    new ArgDoc('width', 'integer?, non-negative'),
    new ArgDoc('height', 'integer?, non-negative'),
    new ArgDoc('mode', 'string?, either `"solid"` or `"outline"`'),
    new ArgDoc('color', 'string?')
  ],
  'Renders a rectangle whose upper-left corner is at `(x, y)`.'
)

export const drawEllipseDoc: Doc = new Doc(
  'draw-ellipse',
  'void?',
  [
    new ArgDoc('canvas', 'canvas?'),
    new ArgDoc('x', 'number?'),
    new ArgDoc('y', 'number?'),
    new ArgDoc('radiusX', 'number?, non-negative'),
    new ArgDoc('radiusY', 'number?, non-negative'),
    new ArgDoc('rotation', 'number?'),
    new ArgDoc('startAngle', 'number?'),
    new ArgDoc('endAngle', 'number?'),
    new ArgDoc('mode', 'string?, either `"solid"` or `"outline"`'),
    new ArgDoc('color', 'string?')
  ],
  'Renders an ellipse whose center is at `(x, y)`, radii `radiusX` and `radiusY`, `rotation`, `startAngle`, and `endAngle`.'
)

export const drawCircleDoc: Doc = new Doc(
  'draw-circle',
  'void?',
  [
    new ArgDoc('canvas', 'canvas?'),
    new ArgDoc('x', 'number?'),
    new ArgDoc('y', 'number?'),
    new ArgDoc('radius', 'number?, non-negative'),
    new ArgDoc('mode', 'string?, either `"solid"` or `"outline"`'),
    new ArgDoc('color', 'string?')
  ],
  'Renders a circle whose center is at `(x, y)` and radius `radius`.'
)

export const drawTextDoc: Doc = new Doc(
  'draw-text',
  'void?',
  [
    new ArgDoc('canvas', 'canvas?'),
    new ArgDoc('text', 'string?'),
    new ArgDoc('x', 'integer?'),
    new ArgDoc('y', 'integer?'),
    new ArgDoc('mode', 'string?, either `"solid"` or `"outline"`'),
    new ArgDoc('color', 'string?'),
    new ArgDoc('font', 'string?, a css font string, e.g., `"24px sans-serif"`')
  ],
  'Renders the given text at the given coordinates.'
)

export const drawDrawingDoc: Doc = new Doc(
  'draw-drawing',
  'void?',
  [
    new ArgDoc('canvas', 'canvas?'),
    new ArgDoc('drawing', 'drawing?'),
    new ArgDoc('x', 'integer?'),
    new ArgDoc('y', 'integer?')
  ],
  'Draws the given drawing (created via the `image` library) at the given coordinates.'
)

export const drawPathDoc: Doc = new Doc(
  'draw-path',
  'void?',
  [
    new ArgDoc('canvas', 'canvas?'),
    new ArgDoc('pairs', 'list?, a list of pairs of numbers'),
    new ArgDoc('mode', 'string?, either `"solid"` or `"outline"`'),
    new ArgDoc('color', 'string?')
  ],
  'Renders a path from the given list of pairs of numbers.'
)

export const animateWithDoc: Doc = new Doc(
  'animate-with',
  'void?',
  [
    new ArgDoc('proc', 'procedure?, a procedure that takes the current time in milliseconds as input.')
  ],
  'Repeatedly calls `proc` approximately once every 60 seconds, creating the effect of animation.'
)

export const canvasOnclickDoc: Doc = new Doc(
  'canvas-onclick',
  'void?',
  [
    new ArgDoc('canvas', 'canvas?'),
    new ArgDoc('proc', 'procedure?, a procedure that takes two arguments: numbers representing the x and y coordinate of the mouse click on the canvas.')
  ],
  'Sets the given procedure to be called when the canvas is clicked by the user.'
)
