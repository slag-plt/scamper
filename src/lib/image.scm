;;; (canvas? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a canvas.
;;; @category canvas, image, typecheck, predicates, html?
(define-export canvas? (js-var "canvas_canvasQ"))

;;; (html? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an HTML element.
;;; @category html, typecheck, predicates, canvas?
(define-export html? (js-var "html_isElement"))

;;; (color? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a valid color: a string containing a named color, an `rgb` value, or an `hsv` value.
;;; @category color, hsv, image, predicates, rgb, typecheck, color-func, find-colors, all-color-names
(define-export color? (js-var "color_colorQ"))

;;; (rgb-component? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an integer between 0 and 255.
;;; @category color, image, predicates, rgb, typecheck, rgb-func, color-func, rgb?, rgb-distance
(define-export rgb-component? (js-var "color_isRgbComponent"))

;;; (rgb? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a rgb value.
;;; @category color, image, predicates, rgb, typecheck, rgb-func, color-func, rgb-component?, rgb-distance
(define-export rgb? (js-var "color_isRgb"))

;;; (rgb r g b & a) -> rgb?
;;;  r : rgb-component?
;;;  g : rgb-component?
;;;  b : rgb-component?
;;;  a : rgb-component?
;;;   optional
;;; Returns an rgb value with the specified components.
;;; @category color, image, rgb, color-func, rgb?, rgb-component?, rgb-distance
(define-export rgb (js-var "color_rgb"))

;;; (rgb-red rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the red component of the rgb value.
;;; @category color, image, rgb, rgb-blue, rgb-green
(define-export rgb-red (js-var "color_rgbRed"))

;;; (rgb-green rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the green component of the rgb value.
;;; @category color, image, rgb, rgb-blue, rgb-red
(define-export rgb-green (js-var "color_rgbGreen"))

;;; (rgb-blue rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the blue component of the rgb value.
;;; @category color, image, rgb, rgb-green, rgb-red
(define-export rgb-blue (js-var "color_rgbBlue"))

;;; (rgb-alpha rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the alpha component of the rgb value.
;;; @category color, image, rgb, rgb-hue, rgb-pseudo-complement, rgb-saturation, rgb-value
(define-export rgb-alpha (js-var "color_rgbAlpha"))

;;; (rgb-distance rgb1 rgb2) -> number?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the Euclidean distance between the two rgb values.
;;; @category color, image, rgb, rgb-func, color-func, rgb?, rgb-component?
(define-export rgb-distance (js-var "color_rgbDistance"))

;;; (color-name? v) -> boolean?
;;;  v : string?
;;; Returns `#t` if and only if `v` is a valid color name.
;;; @category color, image, predicates, typecheck, color-func, all-color-names, find-colors
(define-export color-name? (js-var "color_isColorName"))

;;; (all-color-names x1) -> list?
;;;  x1 : any
;;; Returns a list of all valid color names.
;;; @category color, constants, image, color-func, color?, find-colors, color-name?
(define-export all-color-names (js-var "color_allColorNames"))

;;; (find-colors color-name) -> list?
;;;  color-name : string?
;;; Returns a list of all color names that contain `color`, case-insensitive.
;;; @category image, color-func, color?, all-color-names, color-name?
(define-export find-colors (js-var "color_findColors"))

;;; (rgb->string rgb) -> string?
;;;  rgb : rgb?
;;; Returns a string representation of the rgb value, e.g., approrpiate for use as a shape color.
;;; @category color, image, rgb, color-name->rgb, hsv->rgb, rgb->hsv, hsv->string
(define-export rgb->string (js-var "color_rgbToString"))

;;; (hsv? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a hsv value.
;;; @category color, image, hsv, predicates, typecheck, hsv-func
(define-export hsv? (js-var "color_isHsv"))

;;; (hsv h s v & a) -> hsv?
;;;  h : number?
;;;   0 <= h <= 360
;;;  s : number?
;;;   0 <= s <= 100
;;;  v : number?
;;;   0 <= v <= 100
;;;  a : number?
;;;   0 <= a <= 255
;;; Returns a hsv value with the specified components.
;;; @category color, hsv, image, hsv?
(define-export hsv (js-var "color_hsv"))

;;; (hsv-hue hsv) -> number?
;;;  hsv : hsv?
;;; Returns the hue component of the hsv value.
;;; @category color, hsv, image, hsv-alpha, hsv-complement, hsv-saturation, hsv-value
(define-export hsv-hue (js-var "color_hsvHue"))

;;; (hsv-saturation hsv) -> number?
;;;  hsv : hsv?
;;; Returns the saturation component of the hsv value.
;;; @category color, hsv, image, hsv-alpha, hsv-hue, hsv-complement, hsv-value
(define-export hsv-saturation (js-var "color_hsvSaturation"))

;;; (hsv-value hsv) -> number?
;;;  hsv : hsv?
;;; Returns the value component of the hsv value.
;;; @category color, hsv, image, hsv-alpha, hsv-hue, hsv-complement, hsv-saturation
(define-export hsv-value (js-var "color_hsvValue"))

;;; (hsv-alpha hsv) -> number?
;;;  hsv : hsv?
;;; Returns the alpha component of the hsv value.
;;; @category color, hsv, image, hsv-hue, hsv-complement, hsv-saturation, hsv-value
(define-export hsv-alpha (js-var "color_hsvAlpha"))

;;; (hsv-complement hsv) -> hsv?
;;;  hsv : hsv?
;;; Returns the complement of the hsv value.
;;; @category color, hsv, image, hsv-alpha, hsv-hue, hsv-saturation, hsv-value
(define-export hsv-complement (js-var "color_hsvComplement"))

;;; (rgb-hue rgb) -> number?
;;;  rgb : rgb?
;;; Returns the hue component of the rgb value.
;;; @category color, hsv, image, rgb, rgb-alpha, rgb-pseudo-complement, rgb-saturation, rgb-value
(define-export rgb-hue (js-var "color_rgbHue"))

;;; (rgb-saturation rgb) -> number?
;;;  rgb : rgb?
;;; Returns the saturation component of the rgb value.
;;; @category color, hsv, image, rgb, rgb-alpha, rgb-hue, rgb-pseudo-complement, rgb-value
(define-export rgb-saturation (js-var "color_rgbSaturation"))

;;; (rgb-value rgb) -> number?
;;;  rgb : rgb?
;;; Returns the value component of the rgb value.
;;; @category color, hsv, image, rgb, rgb-alpha, rgb-hue, rgb-pseudo-complement, rgb-saturation
(define-export rgb-value (js-var "color_rgbValue"))

;;; (rgb->hsv rgb) -> hsv?
;;;  rgb : rgb?
;;; Converts the rgb value to an hsv value.
;;; @category color, hsv, image, rgb, color-name->rgb, hsv->rgb, rgb->string
(define-export rgb->hsv (js-var "color_rgbToHsv"))

;;; (hsv->string hsv) -> string?
;;;  hsv : hsv?
;;; Returns a string representation of the hsv value.
;;; @category color, hsv, image, rgb->hsv, hcv->rgb
(define-export hsv->string (js-var "color_hsvToString"))

;;; (color-name->rgb color-name) -> rgb?
;;;  color-name : string?
;;; Returns the rgb value of the color name.
;;; @category color, image, rgb, hsv->rgb, rgb->hsv, rgb->string
(define-export color-name->rgb (js-var "color_colorNameToRgb"))

;;; (hsv->rgb hsv) -> rgb?
;;;  hsv : hsv?
;;; Converts the hsv value to an rgb value.
;;; @category color, hsv, image, rgb, color-name->rgb, rgb->hsv, rgb->string, hsv->string
(define-export hsv->rgb (js-var "color_hsvToRgb"))

;;; (rgb-darker rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a darker version of the rgb value.
;;; @category color, image, rgb, rgb-lighter
(define-export rgb-darker (js-var "color_rgbDarker"))

;;; (rgb-lighter rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a lighter version of the rgb value.
;;; @category color, image, rgb, rgb-lighter
(define-export rgb-lighter (js-var "color_rgbLighter"))

;;; (rgb-redder rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a redder version of the rgb value.
;;; @category color, image, rgb, rgb-bluer, rgb-greener
(define-export rgb-redder (js-var "color_rgbRedder"))

;;; (rgb-bluer rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a bluer version of the rgb value.
;;; @category color, image, rgb, rgb-greener, rgb-redder
(define-export rgb-bluer (js-var "color_rgbBluer"))

;;; (rgb-greener rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a greener version of the rgb value.
;;; @category color, image, rgb, rgb-bluer, rgb-redder
(define-export rgb-greener (js-var "color_rgbGreener"))

;;; (rgb-pseudo-complement rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a pseudo-complement of the rgb value.
;;; @category color, image, rgb, rgb-greyscale, rgb-phaseshift, rgb-rotate-components
(define-export rgb-pseudo-complement (js-var "color_rgbPseudoComplement"))

;;; (rgb-greyscale rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a greyscale version of the rgb value.
;;; @category color, image, rgb, rgb-phaseshift, rgb-rotate-components
(define-export rgb-greyscale (js-var "color_rgbGreyscale"))

;;; (rgb-phaseshift rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a phaseshifted version of the rgb value.
;;; @category color, image, rgb, rgb-greyscale, rgb-rotate-components
(define-export rgb-phaseshift (js-var "color_rgbPhaseshift"))

;;; (rgb-rotate-components rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a rotated version of the rgb value.
;;; @category color, image, rgb, rgb-greyscale, rgb-phaseshift
(define-export rgb-rotate-components (js-var "color_rgbRotateComponents"))

;;; (rgb-thin rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a thinner version of the rgb value.
;;; @category color, image, rgb, rgb-thicken
(define-export rgb-thin (js-var "color_rgbThin"))

;;; (rgb-thicken rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a thicker version of the rgb value.
;;; @category color, image, rgb, rgb-thin
(define-export rgb-thicken (js-var "color_rgbThicken"))

;;; (rgb-add rgb1 rgb2) -> rgb?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the sum of the two rgb values.
;;; @category color, image, rgb, rgb-subtract, rgb-average
(define-export rgb-add (js-var "color_rgbAdd"))

;;; (rgb-subtract rgb1 rgb2) -> rgb?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the difference of the two rgb values.
;;; @category color, image, rgb, rgb-add, rgb-average
(define-export rgb-subtract (js-var "color_rgbSubtract"))

;;; (rgb-average rgb1 rgb2) -> rgb?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the average of the two rgb values.
;;; @category color, image, rgb, rgb-add, rgb-subtract
(define-export rgb-average (js-var "color_rgbAverage"))

;;; (font? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a font.
;;; @category image, typecheck, font, text
(define-export font? (js-var "font_fontQ"))

;;; (font face system-face bold? italic?) -> font?
;;;  face : any
;;;   string? A valid font name
;;;  system-face : any
;;;   string? A generic font family name (optional, default "sans-serif")
;;;  bold? : any
;;;   boolean? (optional, default #f)
;;;  italic? : any
;;;   boolean? (optional, default #f)
;;; Returns a new font value with the given arguments. The `system-face` name is drawn from one of the possible system font families, a list can be found on [MDN (font-family)](https://developer.mozilla.org/en-US/docs/Web/CSS/font-family#syntax)
;;; @category image, font?, text
(define-export font (js-var "font_font"))

;;; (drawing? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a drawing: the kind of value the shape constructors build. A canvas is not a drawing, and neither is a loaded image file.
;;; @category image, predicates, typecheck, shapes, canvas?
(define-export drawing? (js-var "drawing_drawingQ"))

;;; (fill-mode? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a fill mode: the string `"solid"` or `"outline"`.
;;; @category image, shapes, typecheck, predicates, ellipse, rectangle
(define-export fill-mode? (js-var "drawing_fillModeQ"))

;;; (ellipse width height fill color [line-width]) -> drawing?
;;;  width : integer?
;;;  height : integer?
;;;  fill : fill-mode?
;;;  color : color?
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a new drawing containing an ellipse with dimensions `width × height`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-ellipse, outlined-ellipse
(define-export ellipse (js-var "drawing_ellipse"))

;;; (circle diameter fill color [line-width]) -> drawing?
;;;  diameter : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing consisting of a circle `diameter` wide and `diameter` tall, the same size as `(square diameter fill color)`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-circle, outlined-circle
(define-export circle (js-var "drawing_circle"))

;;; (rectangle width height fill color [line-width]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing consisting of a rectangle with dimensions `width × height`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-rectangle, outlined-rectangle
(define-export rectangle (js-var "drawing_rectangle"))

;;; (square width fill color [line-width]) -> drawing?
;;;  width : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing consisting of a square with length `width`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-square, outlined-square
(define-export square (js-var "drawing_square"))

;;; (triangle length fill color [line-width]) -> drawing?
;;;  length : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing consisting of a equilateral triangle with length `length`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-triangle, outlined-triangle
(define-export triangle (js-var "drawing_triangle"))

;;; (isosceles-triangle width height fill color [line-width]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing consisting of a isosceles triangle with base `base` and height `height`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-isosceles-triangle, outlined-isosceles-triangle
(define-export isosceles-triangle (js-var "drawing_isoscelesTriangle"))

;;; (path width height points fill color [line-width]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  points : list?
;;;   a list of points, pairs of numbers
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing with dimensions `width × height` formed by connecting the points in `points` with straight lines. The points are specified as a `pair` of coordinates. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, path, with-dash
(define-export path (js-var "drawing_path"))

;;; (beside & d1) -> drawing?
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other (horizontally).
;;; @category composition/placement, image, beside/align, above, above/align, overlay, overlay/align, overlay/offset, rotate
(define-export beside (js-var "drawing_beside"))

;;; (beside/align align & d1) -> drawing?
;;;  align : string?
;;;   either "top", "center", or "bottom"
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other on the x-axis, aligning them along the y-axis according to `align`.
;;; @category composition/placement, image, beside, above, above/align, overlay, overlay/align, overlay/offset, rotate
(define-export beside/align (js-var "drawing_besideAlign"))

;;; (above & d1) -> drawing?
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other (vertically in descending order).
;;; @category composition/placement, image, beside, beside/align, above/align, overlay, overlay/align, overlay/offset, rotate
(define-export above (js-var "drawing_above"))

;;; (above/align align & d1) -> drawing?
;;;  align : string?
;;;   either "left", "middle", or "right"
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other on the y-axis, aligning them along the x-axis according to `align`.
;;; @category composition/placement, image, beside, beside/align, above, overlay, overlay/align, overlay/offset, rotate
(define-export above/align (js-var "drawing_aboveAlign"))

;;; (overlay & d1) -> drawing?
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other. (`d1` is the topmost drawing).
;;; @category composition/placement, image, beside, beside/align, above, above/align, overlay/align, overlay/offset, rotate
(define-export overlay (js-var "drawing_overlay"))

;;; (overlay/align xAlign yAlign & d1) -> drawing?
;;;  xAlign : string?
;;;   either "left", "middle", or "right"
;;;  yAlign : string?
;;;   either "top", "center", or "bottom"
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other, aligning them according to `xAlign` and `yAlign`.
;;; @category composition/placement, image, beside, beside/align, above, above/align, overlay, overlay/offset, rotate
(define-export overlay/align (js-var "drawing_overlayAlign"))

;;; (overlay/offset dx dy d1 d2) -> drawing?
;;;  dx : number?
;;;  dy : number?
;;;  d1 : drawing?
;;;  d2 : drawing?
;;; Creates a new drawing formed by places the drawing `d1` on top of `d2`, offset by `(dx, dy)`.
;;; @category composition/placement, image, beside, beside/align, above, above/align, overlay, overlay/align, rotate
(define-export overlay/offset (js-var "drawing_overlayOffset"))

;;; (rotate angle d) -> drawing?
;;;  angle : number?
;;;   in degrees
;;;  d : drawing?
;;; Returns a new drawing formed by rotating drawing `d` by `angle` degrees around the center of its bounding box.
;;; @category image, beside, beside/align, above, above/align, overlay, overlay/align, overlay/offset
(define-export rotate (js-var "drawing_rotate"))

;;; (with-dash dash-spec d) -> drawing?
;;;  dash-spec : list?
;;;   a list of numbers
;;;  d : drawing?
;;; Returns a new drawing formed by drawing `d` but with lines drawn according to `dash-spec`. `dash-spec` is an list of numbers where each successive pair of numbers describe the length of a dash and the length of the subsequent gap.
;;; @category canvas, image, shapes, path-func
(define-export with-dash (js-var "drawing_withDash"))

;;; (text str size color & font) -> drawing?
;;;  str : string?
;;;  size : any
;;;   number? A valid font size (in px)
;;;  color : color?
;;;  font : any
;;;   font? (optional, default (font "Arial"))
;;; Returns a new drawing formed by drawing `str` with the given arguments.
;;; @category image, font, font?
(define-export text (js-var "drawing_text"))

;;; (solid-square width color) -> drawing?
;;;  width : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid square with length `width`.
;;; @category image, shapes, square, outlined-square
(define-export solid-square (js-var "drawing_solidSquare"))

;;; (outlined-square width color [line-width]) -> drawing?
;;;  width : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing consisting of an outline square with length `width`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, square, solid-square
(define-export outlined-square (js-var "drawing_outlinedSquare"))

;;; (solid-rectangle width height color) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid rectangle with dimensions `width × height`.
;;; @category image, shapes, rectangle, outlined-rectangle
(define-export solid-rectangle (js-var "drawing_solidRectangle"))

;;; (outlined-rectangle width height color [line-width]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing consisting of an outlined rectangle with dimensions `width × height`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, rectangle, solid-rectangle
(define-export outlined-rectangle (js-var "drawing_outlinedRectangle"))

;;; (solid-circle diameter color) -> drawing?
;;;  diameter : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid circle `diameter` wide and `diameter` tall, the same size as `(solid-square diameter color)`.
;;; @category image, shapes, circle, outlined-circle
(define-export solid-circle (js-var "drawing_solidCircle"))

;;; (outlined-circle diameter color line-width) -> drawing?
;;;  diameter : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn
;;; Returns a drawing consisting of an outlined circle of diameter `diameter`, drawn with a `line-width`-wide outline. The drawing is `diameter` + `line-width` across, since the outline is drawn centred on the circle.
;;; @category image, shapes, circle, solid-circle
(define-export outlined-circle (js-var "drawing_outlinedCircle"))

;;; (solid-ellipse width height color) -> drawing?
;;;  width : integer?
;;;  height : integer?
;;;  color : color?
;;; Returns a new drawing containing a solid ellipse with dimensions `width × height`.
;;; @category image, shapes, ellipse, outlined-ellipse
(define-export solid-ellipse (js-var "drawing_solidEllipse"))

;;; (outlined-ellipse width height color [line-width]) -> drawing?
;;;  width : integer?
;;;  height : integer?
;;;  color : color?
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a new drawing containing an outlined ellipse with dimensions `width × height`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, ellipse, solid-ellipse
(define-export outlined-ellipse (js-var "drawing_outlinedEllipse"))

;;; (solid-triangle length color) -> drawing?
;;;  length : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid equilateral triangle with length `length`.
;;; @category image, shapes, triangle, outlined-triangle
(define-export solid-triangle (js-var "drawing_solidTriangle"))

;;; (outlined-triangle length color [line-width]) -> drawing?
;;;  length : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing consisting of an outlined equilateral triangle with length `length`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, triangle, solid-triangle
(define-export outlined-triangle (js-var "drawing_outlinedTriangle"))

;;; (solid-isosceles-triangle width height color) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid isosceles triangle with base `base` and height `height`.
;;; @category image, shapes, isosceles-triangle, outlined-isosceles-triangle
(define-export solid-isosceles-triangle (js-var "drawing_solidIsoscelesTriangle"))

;;; (outlined-isosceles-triangle width height color [line-width]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;; Returns a drawing consisting of an outlined isosceles triangle with base `base` and height `height`. An outlined shape is `line-width` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, isosceles-triangle, solid-isosceles-triangle
(define-export outlined-isosceles-triangle (js-var "drawing_outlinedIsoscelesTriangle"))

;;; (drawing-width drawing) -> number?
;;;  drawing : drawing?
;;; Returns the width of the drawing.
;;; @category image, drawing-height
(define-export drawing-width (js-var "drawing_drawingWidth"))

;;; (drawing-height drawing) -> number?
;;;  drawing : drawing?
;;; Returns the height of the drawing.
;;; @category image, drawing-width
(define-export drawing-height (js-var "drawing_drawingHeight"))

;;; (drawing-color drawing) -> rgb?
;;;  drawing : drawing?
;;; Returns the color of the drawing. For a composite drawing, this is the average of its parts' colors.
;;; @category image, drawing-recolor
(define-export drawing-color (js-var "drawing_drawingColor"))

;;; (drawing-recolor drawing color) -> drawing?
;;;  drawing : drawing?
;;;  color : color?
;;; Returns a new drawing with the same dimensions as `drawing` but with the color `color`.
;;; @category image, drawing-color
(define-export drawing-recolor (js-var "drawing_drawingRecolor"))

;;; (drawing->pixels d) -> vector?
;;;  d : drawing?
;;; Returns a vector of rgb values corresponding to the pixels of the given drawing.
;;; @category image, pixel, drawing->canvas
(define-export drawing->pixels (js-var "drawing_drawingToPixels"))

;;; (drawing->canvas drawing) -> canvas?
;;;  drawing : drawing?
;;; Renders `drawing` onto a new canvas and returns it.
;;; @category image, pixel, drawing->pixels
(define-export drawing->canvas (js-var "drawing_drawingToCanvas"))

;;; (with-image-file callback) -> html?
;;;  callback : procedure?
;;; Returns a container with a file chooser that, when used, calls `callback` with the selected image and replaces the container's contentsr with the output produced by `callback`.
;;; @category image, with-image-from-url
(define-export with-image-file (js-var "image_withImageFile"))

;;; (with-image-from-url url callback) -> any
;;;  url : string?
;;;  callback : procedure?
;;; Loads the image at `url` and passes it (as a canvas) to `callback`. The output of `callback` is returned (and rendered to the screen if this is a top-level expression).
;;; @category image, with-image-file
(define-export with-image-from-url
  (lambda (url callback)
    (callback ((js-var "image_blockOnFetchImage") url))))

;;; (pixel-map fn canvas) -> canvas?
;;;  fn : procedure?
;;;  canvas : canvas?
;;; Returns a new canvas that is the result of applying `fn` to each pixel (an rgb value) of `canvas`. `canvas` itself is unchanged.
;;; @category image, pixel, canvas-get-pixel, canvas->pixels, pixels->canvas, canvas-set-pixels!
(define-export pixel-map
  (lambda (fn img)
    (pixels->canvas (vector-map fn (canvas->pixels img))
                   (canvas-width img)
                   (canvas-height img))))

;;; (canvas-get-pixel img x y) -> rgb?
;;;  img : drawing?
;;;  x : integer?
;;;  y : integer?
;;; Returns the rgb value of the pixel at position `(x, y)` of `canvas`.
;;; @category color, image, pixel, rgb, pixel-map, canvas->pixels, pixels->canvas, canvas-set-pixels! 
(define-export canvas-get-pixel (js-var "canvas_canvasGetPixel"))

;;; (pixels? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a vector of `rgb` values, the representation `canvas->pixels` produces and `pixels->canvas` consumes.
;;; @category image, pixel, typecheck, predicates, canvas->pixels, pixels->canvas
(define-export pixels? (js-var "canvas_pixelsQ"))

;;; (canvas->pixels canvas) -> pixels?
;;;  canvas : canvas?
;;; Returns the pixels of `canvas` as a vector of `rgb` values, read left-to-right and top-to-bottom. The result is a snapshot: changing it does not change `canvas`. Use `canvas-set-pixels!` to write pixels back.
;;; @category image, pixel-map, canvas-get-pixel, pixels->canvas, canvas-set-pixels! 
(define-export canvas->pixels (js-var "canvas_canvasToPixels"))

;;; (pixels->canvas pixels width height) -> canvas?
;;;  pixels : pixels?
;;;  width : integer?
;;;  height : integer?
;;; Returns a new canvas with the given `pixels` and dimensions `width × height`.
;;; @category image, pixel, pixel-map, canvas-get-pixel, canvas->pixels, canvas-set-pixels! 
(define-export pixels->canvas (js-var "canvas_pixelsToCanvas"))

;;; (canvas-set-pixels! canvas pixels) -> void?
;;;  canvas : canvas?
;;;  pixels : pixels?
;;; Sets the pixels of `canvas` to `pixels`, mutating it in place.
;;; @category canvas, image, mutation, pixel, predicates, pixel-map, canvas-get-pixel, canvas->pixels, pixels->canvas
(define-export canvas-set-pixels! (js-var "canvas_canvasSetPixels"))

;;; (canvas-width canvas) -> integer?
;;;  canvas : canvas?
;;; Returns the width of the canvas in pixels.
;;; @category canvas, image
(define-export canvas-width (js-var "canvas_canvasWidth"))

;;; (canvas-height canvas) -> integer?
;;;  canvas : canvas?
;;; Returns the height of the canvas in pixels.
;;; @category canvas, image
(define-export canvas-height (js-var "canvas_canvasHeight"))
