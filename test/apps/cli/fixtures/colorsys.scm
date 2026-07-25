; Regression for #265: colorsys is a CommonJS module, and under the CLI's
; Node/tsx ESM interop its exports land on the namespace default. rgb->hsv and
; hsv->rgb both route through colorsys, so they threw
; "colorsys.rgbToHsv is not a function" here until color.ts switched to a
; default import. This tier is the one that actually exercises that path (the
; Vite/jsdom suite resolves colorsys regardless).
(import image)
(rgb->hsv (rgb 255 0 0))
(hsv->rgb (hsv 0 100 100))
