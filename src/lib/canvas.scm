;;; (canvas? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a canvas.
;;; @category canvas, image, typecheck, make-canvas
(define canvas? (js-var "canvas_canvasQ"))

;;; (color? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a valid color: a string containing a named color, an `rgb` value, or an `hsv` value.
;;; @category color, hsv, image, predicates, rgb, typecheck
(define color? (js-var "image_colorQ"))

;;; (image? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is an image.
;;; @category image, predicates, typecheck
(define image? (js-var "image_drawingQ"))

;;; (make-canvas width height) -> canvas?
;;;  width : integer?
;;;   positive
;;;  height : integer?
;;;   positive
;;; Creates a canvas with the given width and height.
;;; @category canvas, canvas?
(define make-canvas (js-var "canvas_makeCanvas"))

;;; (canvas-rectangle! canvas x y width height mode color) -> void?
;;;  canvas : canvas?
;;;  x : integer?
;;;  y : integer?
;;;  width : integer?
;;;   non-negative
;;;  height : integer?
;;;   non-negative
;;;  mode : string?
;;;   either `"solid"` or `"outline"`
;;;  color : color?
;;; Renders a rectangle whose upper-left corner is at `(x, y)`.
;;; @category canvas, mutation, predicates, shapes, canvas-ellipse!, canvas-circle!
(define canvas-rectangle! (js-var "canvas_canvasRectangle"))

;;; (canvas-ellipse! canvas x y radiusX radiusY rotation startAngle endAngle mode color) -> void?
;;;  canvas : canvas?
;;;  x : number?
;;;  y : number?
;;;  radiusX : number?
;;;   non-negative
;;;  radiusY : number?
;;;   non-negative
;;;  rotation : number?
;;;  startAngle : number?
;;;  endAngle : number?
;;;  mode : string?
;;;   either `"solid"` or `"outline"`
;;;  color : color?
;;; Renders an ellipse whose center is at `(x, y)`, radii `radiusX` and `radiusY`, `rotation`, `startAngle`, and `endAngle`.
;;; @category canvas, mutation, predicates, shapes, canvas-rectangle!, canvas-circle!
(define canvas-ellipse! (js-var "canvas_canvasEllipse"))

;;; (canvas-circle! canvas x y radius mode color) -> void?
;;;  canvas : canvas?
;;;  x : number?
;;;  y : number?
;;;  radius : number?
;;;   non-negative
;;;  mode : string?
;;;   either `"solid"` or `"outline"`
;;;  color : color?
;;; Renders a circle whose center is at `(x, y)` and radius `radius`.
;;; @category canvas, mutation, predicates, shapes, canvas-rectangle!, canvas-ellipse!
(define canvas-circle! (js-var "canvas_canvasCircle"))

;;; (canvas-text! canvas x y text size mode color & font) -> void?
;;;  canvas : canvas?
;;;  x : integer?
;;;  y : integer?
;;;  text : string?
;;;  size : number?
;;;   positive
;;;  mode : string?
;;;   either `"solid"` or `"outline"`
;;;  color : color?
;;;  font : string?
;;;   a css font string, e.g., `"24px sans-serif"`
;;; Renders the given text at the given coordinates.
;;; @category canvas, mutation, predicates, canvas-drawing!, canvas-path!
(define canvas-text! (js-var "canvas_canvasText"))

;;; (canvas-drawing! canvas x y drawing) -> void?
;;;  canvas : canvas?
;;;  x : integer?
;;;  y : integer?
;;;  drawing : image?
;;; Draws the given drawing (created via the `image` library) at the given coordinates.
;;; @category canvas, mutation, predicates, canvas-text!, canvas-path!
(define canvas-drawing! (js-var "canvas_canvasDrawing"))

;;; (canvas-path! canvas pairs mode color) -> void?
;;;  canvas : canvas?
;;;  pairs : list?
;;;   a list of pairs of numbers
;;;  mode : string?
;;;   either `"solid"` or `"outline"`
;;;  color : color?
;;; Renders a path from the given list of pairs of numbers.
;;; @category canvas, mutation, path, predicates, canvas-text!, canvas-drawing!
(define canvas-path! (js-var "canvas_canvasPath"))

;;; (animate-with proc) -> void?
;;;  proc : procedure?
;;;   a procedure that takes the current time in milliseconds as input.
;;; Repeatedly calls `proc` approximately once every 60 seconds, creating the effect of animation. `proc` should return a boolean. If `proc` returns `#t` the loop of calls continues, otherwise, it stops.
;;; @category canvas, canvas-onclick!
(define animate-with (js-var "canvas_animateWith"))

;;; (canvas-onclick! canvas proc) -> void?
;;;  canvas : canvas?
;;;  proc : procedure?
;;;   a procedure that takes two arguments: numbers representing the x and y coordinate of the mouse click on the canvas.
;;; Sets the given procedure to be called when the canvas is clicked by the user.
;;; @category canvas, mutation, predicates, animate-with-proc
(define canvas-onclick! (js-var "canvas_canvasOnclick"))
