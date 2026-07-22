;;; (canvas? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a canvas.
(define canvas? (js-var "canvas_canvasQ"))

;;; (html? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an HTML element.
(define html? (js-var "html_isElement"))

;;; (color r b g a) -> string?
;;;  r : integer?
;;;   0 <= r <= 255
;;;  b : integer?
;;;   0 <= b <= 255
;;;  g : integer?
;;;   0 <= g <= 255
;;;  a : integer?
;;;   0 <= a <= 1
;;; Returns a string of the form `"rgba(r, g, b, a)"` appropriate for use as a color.
;;; @category color, hsv, image, rgb
(define color (js-var "image_color"))

;;; (color? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a valid color: a string containing a named color, an `rgb` value, or an `hsv` value.
;;; @category color, hsv, image, predicates, rgb, typecheck
(define color? (js-var "image_colorQ"))

;;; (rgb-component? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an integer between 0 and 255.
;;; @category color, image, predicates, rgb, typecheck
(define rgb-component? (js-var "image_isRgbComponent"))

;;; (rgb? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a rgb value.
;;; @category color, image, predicates, rgb, typecheck
(define rgb? (js-var "image_isRgb"))

;;; (rgb r g b . a) -> rgb?
;;;  r : rgb-component?
;;;  g : rgb-component?
;;;  b : rgb-component?
;;;  a : rgb-component?
;;;   optional
;;; Returns an rgb value with the specified components.
;;; @category color, image, rgb
(define rgb (js-var "image_rgb"))

;;; (rgb-red rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the red component of the rgb value.
;;; @category color, image, rgb
(define rgb-red (js-var "image_rgbRed"))

;;; (rgb-green rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the green component of the rgb value.
;;; @category color, image, rgb
(define rgb-green (js-var "image_rgbGreen"))

;;; (rgb-blue rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the blue component of the rgb value.
;;; @category color, image, rgb
(define rgb-blue (js-var "image_rgbBlue"))

;;; (rgb-alpha rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the alpha component of the rgb value.
;;; @category color, image, rgb
(define rgb-alpha (js-var "image_rgbAlpha"))

;;; (rgb-distance rgb1 rgb2) -> number?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the Euclidean distance between the two rgb values.
;;; @category color, image, rgb
(define rgb-distance (js-var "image_rgbDistance"))

;;; (color-name? v) -> boolean?
;;;  v : string
;;; Returns `#t` if and only if `v` is a valid color name.
;;; @category color, image, predicates, typecheck
(define color-name? (js-var "image_isColorName"))

;;; (all-color-names x1) -> list?
;;;  x1 : any
;;; Returns a list of all valid color names.
;;; @category color, constants, image
(define all-color-names (js-var "image_allColorNames"))

;;; (find-colors color-name) -> list?
;;;  color-name : string
;;; Returns a list of all color names that contain `color`, case-insensitive.
;;; @category image
(define find-colors (js-var "image_findColors"))

;;; (rgb->string rgb) -> string?
;;;  rgb : rgb?
;;; Returns a string representation of the rgb value, e.g., approrpiate for use as a shape color.
;;; @category color, image, rgb
(define rgb->string (js-var "image_rgbToString"))

;;; (hsv? . v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a hsv value.
;;; @category color, image, hsv, predicates, typecheck
(define hsv? (js-var "image_hsv"))

;;; (hsv h s v . a) -> hsv?
;;;  h : number?
;;;   0 <= h <= 360
;;;  s : number?
;;;   0 <= s <= 100
;;;  v : number?
;;;   0 <= v <= 100
;;;  a : number?
;;;   0 <= a <= 255
;;; Returns a hsv value with the specified components.
;;; @category color, hsv, image
(define hsv (js-var "image_hsv"))

;;; (hsv-hue hsv) -> number?
;;;  hsv : hsv?
;;; Returns the hue component of the hsv value.
;;; @category color, hsv, image
(define hsv-hue (js-var "image_hsvHue"))

;;; (hsv-saturation hsv) -> number?
;;;  hsv : hsv?
;;; Returns the saturation component of the hsv value.
;;; @category color, hsv, image
(define hsv-saturation (js-var "image_hsvSaturation"))

;;; (hsv-value hsv) -> number?
;;;  hsv : hsv?
;;; Returns the value component of the hsv value.
;;; @category color, hsv, image
(define hsv-value (js-var "image_hsvValue"))

;;; (hsv-alpha hsv) -> number?
;;;  hsv : hsv?
;;; Returns the alpha component of the hsv value.
;;; @category color, hsv, image
(define hsv-alpha (js-var "image_hsvAlpha"))

;;; (hsv-complement hsv) -> hsv?
;;;  hsv : hsv?
;;; Returns the complement of the hsv value.
;;; @category color, hsv, image
(define hsv-complement (js-var "image_hsvComplement"))

;;; (rgb-hue rgb) -> number?
;;;  rgb : rgb?
;;; Returns the hue component of the rgb value.
;;; @category color, hsv, image, rgb
(define rgb-hue (js-var "image_rgbHue"))

;;; (rgb-saturation rgb) -> number?
;;;  rgb : rgb?
;;; Returns the saturation component of the rgb value.
;;; @category color, hsv, image, rgb
(define rgb-saturation (js-var "image_rgbSaturation"))

;;; (rgb-value rgb) -> number?
;;;  rgb : rgb?
;;; Returns the value component of the rgb value.
;;; @category color, hsv, image, rgb
(define rgb-value (js-var "image_rgbValue"))

;;; (rgb->hsv rgb) -> hsv?
;;;  rgb : rgb?
;;; Converts the rgb value to an hsv value.
;;; @category color, hsv, image, rgb
(define rgb->hsv (js-var "image_rgbToHsv"))

;;; (hsv->string hsv) -> string?
;;;  hsv : hsv?
;;; Returns a string representation of the hsv value.
;;; @category color, hsv, image
(define hsv->string (js-var "image_hsvToString"))

;;; (color-name->rgb color-name) -> rgb?
;;;  color-name : string
;;; Returns the rgb value of the color name.
;;; @category color, image, rgb
(define color-name->rgb (js-var "image_colorNameToRgb"))

;;; (hsv->rgb hsv) -> rgb?
;;;  hsv : hsv?
;;; Converts the hsv value to an rgb value.
;;; @category color, hsv, image, rgb
(define hsv->rgb (js-var "image_hsvToRgb"))

;;; (rgb-darker rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a darker version of the rgb value.
;;; @category color, image, rgb
(define rgb-darker (js-var "image_rgbDarker"))

;;; (rgb-lighter rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a lighter version of the rgb value.
;;; @category color, image, rgb
(define rgb-lighter (js-var "image_rgbLighter"))

;;; (rgb-redder rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a redder version of the rgb value.
;;; @category color, image, rgb
(define rgb-redder (js-var "image_rgbRedder"))

;;; (rgb-bluer rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a bluer version of the rgb value.
;;; @category color, image, rgb
(define rgb-bluer (js-var "image_rgbBluer"))

;;; (rgb-greener rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a greener version of the rgb value.
;;; @category color, image, rgb
(define rgb-greener (js-var "image_rgbGreener"))

;;; (rgb-pseudo-complement rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a pseudo-complement of the rgb value.
;;; @category color, image, rgb
(define rgb-pseudo-complement (js-var "image_rgbPseudoComplement"))

;;; (rgb-greyscale rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a greyscale version of the rgb value.
;;; @category color, image, rgb
(define rgb-greyscale (js-var "image_rgbGreyscale"))

;;; (rgb-phaseshift rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a phaseshifted version of the rgb value.
;;; @category color, image, rgb
(define rgb-phaseshift (js-var "image_rgbPhaseshift"))

;;; (rgb-rotate-components rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a rotated version of the rgb value.
;;; @category color, image, rgb
(define rgb-rotate-components (js-var "image_rgbRotateComponents"))

;;; (rgb-thin rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a thinner version of the rgb value.
;;; @category color, image, rgb
(define rgb-thin (js-var "image_rgbThin"))

;;; (rgb-thicken rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a thicker version of the rgb value.
;;; @category color, image, rgb
(define rgb-thicken (js-var "image_rgbThicken"))

;;; (rgb-add rgb1 rgb2) -> rgb?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the sum of the two rgb values.
;;; @category color, image, rgb
(define rgb-add (js-var "image_rgbAdd"))

;;; (rgb-subtract rgb1 rgb2) -> rgb?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the difference of the two rgb values.
;;; @category color, image, rgb
(define rgb-subtract (js-var "image_rgbSubtract"))

;;; (rgb-average rgb1 rgb2) -> rgb?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the average of the two rgb values.
;;; @category color, image, rgb
(define rgb-average (js-var "image_rgbAverage"))

;;; (font? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a font.
(define font? (js-var "image_fontQ"))

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
;;; @category image
(define font (js-var "image_font"))

;;; (image? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is an image.
;;; @category image, predicates, typecheck
(define image? (js-var "image_drawingQ"))

;;; (shape? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a shape.
;;; @category image, predicates, shapes, typecheck
(define shape? (js-var "image_drawingQ"))

;;; (ellipse width height fill color) -> image?
;;;  width : integer?
;;;  height : integer?
;;;  fill : boolean?
;;;  color : string?
;;; Returns a new drawing containing an ellipse with dimensions `width × height`.
;;; @category image, shapes
(define ellipse (js-var "image_ellipse"))

;;; (circle radius fill color) -> image?
;;;  radius : number?
;;;  fill : string?
;;;   either "solid" or "outline"
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a circle of radius `radius`.
;;; @category image, shapes
(define circle (js-var "image_circle"))

;;; (rectangle width height fill color) -> image?
;;;  width : number?
;;;  height : number?
;;;  fill : string?
;;;   either "solid" or "outline"
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a rectangle with dimensions `width × height`.
;;; @category image, shapes
(define rectangle (js-var "image_rectangle"))

;;; (square width fill color) -> image?
;;;  width : number?
;;;  fill : string?
;;;   either "solid" or "outline"
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a square with length `width`.
;;; @category image, shapes
(define square (js-var "image_square"))

;;; (triangle length fill color) -> image?
;;;  length : number?
;;;  fill : string?
;;;   either "solid" or "outline"
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a equilateral triangle with length `length`.
;;; @category triangle, shapes
(define triangle (js-var "image_triangle"))

;;; (isosceles-triangle width height fill color) -> image?
;;;  width : number?
;;;  height : number?
;;;  fill : string?
;;;   either "solid" or "outline"
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a isosceles triangle with base `base` and height `height`.
;;; @category image, shapes
(define isosceles-triangle (js-var "image_isoscelesTriangle"))

;;; (path width height points fill color) -> image?
;;;  width : number?
;;;  height : number?
;;;  points : list?
;;;   a list of points, pairs of numbers
;;;  fill : string?
;;;   either "solid" or "outline"
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing with dimensions `width × height` formed by connecting the points in `points` with straight lines. The points are specified as a `pair` of coordinates.
;;; @category image, path
(define path (js-var "image_path"))

;;; (beside . d1) -> image?
;;;  d1 : image?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other (horizontally).
;;; @category composition/placement, image
(define beside (js-var "image_beside"))

;;; (beside/align align . d1) -> image?
;;;  align : string?
;;;   either "top", "center", or "bottom"
;;;  d1 : image?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other on the x-axis, aligning them along the y-axis according to `align`.
;;; @category composition/placement, image
(define beside/align (js-var "image_besideAlign"))

;;; (above . d1) -> image?
;;;  d1 : image?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other (vertically in descending order).
;;; @category composition/placement, image
(define above (js-var "image_above"))

;;; (above/align align . d1) -> image?
;;;  align : string?
;;;   either "left", "middle", or "right"
;;;  d1 : image?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other on the y-axis, aligning them along the x-axis according to `align`.
;;; @category composition/placement, image
(define above/align (js-var "image_aboveAlign"))

;;; (overlay . d1) -> image?
;;;  d1 : image?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other. (`d1` is the topmost drawing).
;;; @category composition/placement, image
(define overlay (js-var "image_overlay"))

;;; (overlay/align xAlign yAlign . d1) -> image?
;;;  xAlign : string?
;;;   either "left", "middle", or "right"
;;;  yAlign : string?
;;;   either "top", "center", or "bottom"
;;;  d1 : image?
;;; Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other, aligning them according to `xAlign` and `yAlign`.
;;; @category composition/placement, image
(define overlay/align (js-var "image_overlayAlign"))

;;; (overlay/offset dx dy d1 d2) -> image?
;;;  dx : number?
;;;  dy : number?
;;;  d1 : image?
;;;  d2 : image?
;;; Creates a new drawing formed by places the drawing `d1` on top of `d2`, offset by `(dx, dy)`.
;;; @category composition/placement, image
(define overlay/offset (js-var "image_overlayOffset"))

;;; (rotate angle d) -> image?
;;;  angle : number?
;;;   in degrees
;;;  d : image?
;;; Returns a new drawing formed by rotating drawing `d` by `angle` degrees around the center of its bounding box. Note: currently buggy and shifts off-center.
;;; @category image
(define rotate (js-var "image_rotate"))

;;; (with-dash dash-spec d) -> image?
;;;  dash-spec : list?
;;;   a list of numbers
;;;  d : image?
;;; Returns a new drawing formed by drawing `d` but with lines drawn according to `dash-spec`. `dash-spec` is an list of numbers where each successive pair of numbers describe the length of a dash and the length of the subsequent gap.
;;; @category canvas, image, shapes
(define with-dash (js-var "image_withDash"))

;;; (text str size color . font) -> image?
;;;  str : string?
;;;  size : any
;;;   number? A valid font size (in px)
;;;  color : color?
;;;  font : any
;;;   font? (optional, default (font "Arial"))
;;; Returns a new drawing formed by drawing `str` with the given arguments.
;;; @category image
(define text (js-var "image_text"))

;;; (solid-square width color) -> image?
;;;  width : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid square with length `width`.
;;; @category image, shapes
(define solid-square (js-var "image_solidSquare"))

;;; (outlined-square width color) -> image?
;;;  width : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of an outline square with length `width`.
;;; @category image, shapes
(define outlined-square (js-var "image_outlinedSquare"))

;;; (solid-rectangle width height color) -> image?
;;;  width : number?
;;;  height : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid rectangle with dimensions `width × height`.
;;; @category image, shapes
(define solid-rectangle (js-var "image_solidRectangle"))

;;; (outlined-rectangle width height color) -> image?
;;;  width : number?
;;;  height : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of an outlined rectangle with dimensions `width × height`.
;;; @category image, shapes
(define outlined-rectangle (js-var "image_outlinedRectangle"))

;;; (solid-circle radius color) -> image?
;;;  radius : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid circle of radius `radius`.
;;; @category image, shapes
(define solid-circle (js-var "image_solidCircle"))

;;; (outlined-circle radius color) -> image?
;;;  radius : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of an outlined circle of radius `radius`.
;;; @category image, shapes
(define outlined-circle (js-var "image_outlinedCircle"))

;;; (solid-ellipse width height color) -> image?
;;;  width : integer?
;;;  height : integer?
;;;  color : string?
;;; Returns a new drawing containing a solid ellipse with dimensions `width × height`.
;;; @category image, shapes
(define solid-ellipse (js-var "image_solidEllipse"))

;;; (outlined-ellipse width height color) -> image?
;;;  width : integer?
;;;  height : integer?
;;;  color : string?
;;; Returns a new drawing containing an outlined ellipse with dimensions `width × height`.
;;; @category image, shapes
(define outlined-ellipse (js-var "image_outlinedEllipse"))

;;; (solid-triangle length color) -> image?
;;;  length : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid equilateral triangle with length `length`.
;;; @category image, shapes
(define solid-triangle (js-var "image_solidTriangle"))

;;; (outlined-triangle length color) -> image?
;;;  length : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of an outlined equilateral triangle with length `length`.
;;; @category image, shapes
(define outlined-triangle (js-var "image_outlinedTriangle"))

;;; (solid-isosceles-triangle width height color) -> image?
;;;  width : number?
;;;  height : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of a solid isosceles triangle with base `base` and height `height`.
;;; @category image, shapes
(define solid-isosceles-triangle (js-var "image_solidIsoscelesTriangle"))

;;; (outlined-isosceles-triangle width height color) -> image?
;;;  width : number?
;;;  height : number?
;;;  color : string?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;; Returns a drawing consisting of an outlined isosceles triangle with base `base` and height `height`.
;;; @category image, shapes
(define outlined-isosceles-triangle (js-var "image_outlinedIsoscelesTriangle"))

;;; (image-width img) -> number?
;;;  img : image?
;;; Returns the width of the image.
;;; @category image
(define image-width (js-var "image_imageWidth"))

;;; (image-height img) -> number?
;;;  img : image?
;;; Returns the height of the image.
;;; @category image
(define image-height (js-var "image_imageHeight"))

;;; (image-color img) -> string?
;;;  img : image?
;;; Returns the color of the image.
;;; @category image
(define image-color (js-var "image_imageColor"))

;;; (image-recolor img color) -> image?
;;;  img : image?
;;;  color : color?
;;; Returns a new image with the same dimensions as `img` but with the color `color`.
;;; @category image
(define image-recolor (js-var "image_imageRecolor"))

;;; (drawing->pixels d) -> vector?
;;;  d : image?
;;; Returns a vector of rgb values corresponding to the pixels of the given drawing.
;;; @category image, pixel
(define drawing->pixels (js-var "image_drawingToPixels"))

;;; (drawing->image drawing) -> image?
;;;  drawing : image?
;;; Returns a new image/canvas created from the provided drawing.
;;; @category image, pixel
(define drawing->image (js-var "image_drawingToImage"))

;;; (with-image-file callback) -> html?
;;;  callback : procedure?
;;; Returns a container with a file chooser that, when used, calls `callback` with the selected image and replaces the container's contentsr with the output produced by `callback`.
;;; @category image
(define with-image-file (js-var "image_withImageFile"))

;;; (with-image-from-url url callback) -> html?
;;;  url : string?
;;;  callback : procedure?
;;; Returns a container that, when used, calls `callback` with the selected image and replaces the chooser with the output produced by `callback`.
;;; @category image
(define with-image-from-url (js-var "image_withImageFromUrl"))

;;; (pixel-map fn img) -> image?
;;;  fn : procedure?
;;;  img : image?
;;; Returns a new `img` that is the result of applying `fn` to each pixel (represented as a rgb value) of the original `img`.
;;; @category image, pixel
(define pixel-map (js-var "image_pixelMap"))

;;; (image-get-pixel img x y) -> rgb?
;;;  img : image?
;;;  x : integer?
;;;  y : integer?
;;; Returns the rgb value of the pixel at position `(x, y)` in the image.
;;; @category color, image, pixel, rgb
(define image-get-pixel (js-var "image_imageGetPixel"))

;;; (image->pixels img) -> canvas?
;;;  img : canvas?
;;; Returns a vector of `rgb` values corresponding to the pixels of the given canvas.
;;; @category image
(define image->pixels (js-var "image_imageToPixels"))

;;; (pixels->image pixels width height) -> canvas?
;;;  pixels : any
;;;   vector? of rgb values
;;;  width : integer?
;;;  height : integer?
;;; Returns a new canvas with the given `pixels` and dimensions `width × height`.
;;; @category image, pixel
(define pixels->image (js-var "image_pixelsToImage"))

;;; (canvas-set-pixels! canvas pixels) -> void?
;;;  canvas : canvas?
;;;  pixels : any
;;;   vector? of rgb values
;;; Sets the pixels of the given `canvas` to the given `pixels`.
;;; @category canvas, image, mutation, pixel, predicates
(define canvas-set-pixels! (js-var "image_canvasSetPixels"))
