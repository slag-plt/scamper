import Doc from './docs.js'

export const makeCanvasDoc: Doc = new Doc(
  '(make-canvas width height) -> canvas?', [
    'width: integer?, positive',
    'height: integer?, positive'
  ],
  'Creates a canvas with the given width and height.'
)

export const drawRectangleDoc: Doc = new Doc(
  '(draw-rectangle canvas x y width height) -> void?', [
    'canvas: canvas?',
    'x: integer?',
    'y: integer?',
    'width: integer?, non-negative',
    'height: integer?, non-negative',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?'
  ],
  'Renders a rectangle whose upper-left corner is at `(x, y)`.'
)

export const drawEllipseDoc = new Doc(
  '(draw-ellipse canvas x y radiusX radiusY rotation startAngle endAngle mode color) -> void?', [
    'canvas: canvas?',
    'x: number?',
    'y: number?',
    'radiusX: number?, non-negative',
    'radiusY: number?, non-negative',
    'rotation: number?',
    'startAngle: number?',
    'endAngle: number?',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?'
  ],
  'Renders an ellipse whose center is at `(x, y)`, radii `radiusX` and `radiusY`, `rotation`, and `startAngle`, and `endAngle`.'
)

export const drawCircleDoc = new Doc(
  '(draw-circle canvas x y radius mode color) -> void?', [
    'canvas: canvas?',
    'x: number?',
    'y: number?',
    'radius: number?, non-negative',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?'
  ],
  'Renders a circle whose center is at `(x, y)` and radius `radius`.'
)

export const drawTextDoc: Doc = new Doc(
  '(draw-text canvas text x y mode color font) -> void?', [
    'canvas: canvas?',
    'text: string?',
    'x: integer?',
    'y: integer?',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?',
    'font: string", a css font string, e.g., `"24px sans-serif"`'
  ],
  'Renders the given text at the given coordinates.'
)

export const drawDrawingDoc: Doc = new Doc(
  '(draw-drawing canvas drawing x y) -> void?', [
    'canvas: canvas?',
    'drawing: drawing?',
    'x: integer?',
    'y: integer?'
  ],
  'Draws the given drawing (created via the `image` library) at the given coordinates.'
)

export const drawPathDoc: Doc = new Doc(
  '(draw-path canvas pairs mode color) -> void?', [
    'canvas: canvas?',
    'pairs: list?, a list of pairs of numbers',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?'
  ],
  'Renders a path from the given list of pairs of numbers.'
)

export const animateWithDoc: Doc = new Doc(
  '(animate-with proc) -> void?', [
    'proc: procedure?, a procedure that takes the current time in milliseconds as input.'
  ],
  'Repeatedly calls `proc` approximately once every 60 seconds, creating the effect of animation.'
)

export const canvasOnclickDoc: Doc = new Doc(
  '(canvas-onclick canvas proc) -> void?', [
    'canvas: canvas?',
    'proc: procedure?, a procedure that takes two arguments: numbers representing the x and y coordinate of the mouse click on the canvas.'
  ],
  'Sets the given procedure to be called when the canvas is clicked by the user.'
)
