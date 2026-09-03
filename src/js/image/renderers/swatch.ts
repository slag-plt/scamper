import { Rgb, color_rgb } from '../color.js'

// How a color swatch is painted (issue #456). This is presentation, so it
// lives with the renderers rather than in `color.ts`, which mirrors the
// library surface students call.

/**
 * The opaque ground a swatch is painted over. It is fixed white rather than
 * the page's own background so that a swatch carries its own ground: the
 * swatch, and the label chosen for it, look the same in both themes and
 * nothing has to be re-rendered when the theme is toggled.
 */
const swatchBacking: Rgb = color_rgb(255, 255, 255, 255)

/**
 * Composites `color` over `swatchBacking` by its alpha, giving the opaque
 * color the swatch is actually painted in.
 */
export function swatchFill (color: Rgb): Rgb {
  const alpha = color.alpha / 255
  const over = (c: number, backing: number) =>
    Math.round(alpha * c + (1 - alpha) * backing)
  return color_rgb(
    over(color.red, swatchBacking.red),
    over(color.green, swatchBacking.green),
    over(color.blue, swatchBacking.blue),
    255
  )
}

/** The WCAG relative luminance of an opaque color, in [0, 1]. */
function relativeLuminance (color: Rgb): number {
  const linear = (component: number) => {
    const c = component / 255
    return c <= 0.03928 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4)
  }
  return 0.2126 * linear(color.red) +
    0.7152 * linear(color.green) +
    0.0722 * linear(color.blue)
}

/**
 * The luminance at which black and white label a swatch equally well. WCAG
 * contrast is `(L₁ + 0.05) / (L₂ + 0.05)`, so black and white tie when
 * `(L + 0.05) / 0.05 = 1.05 / (L + 0.05)`, i.e. `L = sqrt(1.05 * 0.05) - 0.05`.
 */
const inkThreshold = Math.sqrt(1.05 * 0.05) - 0.05

/**
 * The label color for a swatch of `color`: plain black or plain white,
 * whichever contrasts more with the swatch as it is actually painted.
 */
export function swatchInk (color: Rgb): '#000000' | '#ffffff' {
  return relativeLuminance(swatchFill(color)) > inkThreshold ? '#000000' : '#ffffff'
}
