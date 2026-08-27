import * as L from '../../lpm'

import colorsys from 'colorsys'

// NOTE: throughout the image library, we standardize on the `Rgb` struct as
//       the representation for colors. It is the responsibility of the various
//       data structures in the library to internally store colors as `Rgb`
//       values and accept multiple representations as input, converting via
//       `colorToRgb` as necessary.

/***** Generic Colors *********************************************************/

// N.B., `color` -- a legacy constructor from the htdp library -- was retired in
// #103. It was a pure alias for `rgb` whose documentation had never matched it:
// it listed its parameters as `r b g a` (the implementation took r, g, b, a),
// declared a `string?` return while returning an rgb, and capped alpha at 1.
// Rgb remains the stored color type for shapes; use `rgb` to build one.

/**
 * What a library function takes where it wants a colour: a CSS colour name, an
 * `rgb`, or an `hsv`. `color_colorToRgb` is what turns one into the `Rgb` that
 * shapes actually store.
 */
export type Color = string | Rgb | Hsv

/** Converts between various representations of color in Scamper. */
export function color_colorToRgb (v: L.Value): Rgb {
  if (L.isStructKind<Rgb>(v, 'rgba')) {
    return v
  } else if (typeof v === 'string') {
    return color_colorNameToRgb(v)
  } else if (L.isStructKind<Hsv>(v, 'hsv')) {
    return color_hsvToRgb(v)
  } else {
    throw new L.ScamperError('Runtime', `Shapes expect a valid color, received a: ${L.typeOf(v)}`)
  }
}

export function color_colorQ (v: L.Value): boolean {
  return (typeof v === 'string' && color_isColorName(v)) ||
    L.isStructKind(v, 'rgba') ||
    L.isStructKind(v, 'hsv')
}

/***** RGB(A) Colors **********************************************************/

export interface Rgb extends L.Struct {
  [L.structKind]: 'rgba'
  red: number
  green: number
  blue: number
  alpha: number
}

export function color_isRgbComponent(n: number): boolean {
  return n >= 0 && n <= 255
}

export function color_isRgb (v: L.Value): boolean {
  return L.isStructKind(v, 'rgba')
}

export function color_rgb(...args: number[]): Rgb {
  if (args.length !== 3 && args.length !== 4) {
    throw new L.ScamperError('Runtime', `rgb: expects 3 or 4 arguments, but got ${args.length}`)
  }
  const red = Math.max(0, Math.min(args[0], 255))
  const green = Math.max(0, Math.min(args[1], 255))
  const blue = Math.max(0, Math.min(args[2], 255))
  const alpha = args[3] ?? 255
  return ({
    [L.scamperTag]: 'struct', [L.structKind]: 'rgba',
    red, green, blue, alpha
  })
}

export function color_rgbRed(rgba: Rgb): number {
  return rgba.red
}

export function color_rgbGreen(rgba: Rgb): number {
  return rgba.green
}

export function color_rgbBlue(rgba: Rgb): number {
  return rgba.blue
}

export function color_rgbAlpha(rgba: Rgb): number {
  return rgba.alpha
}

export function color_rgbDistance(rgba1: Rgb, rgba2: Rgb): number {
  return Math.sqrt(
    Math.pow(rgba1.red - rgba2.red, 2) +
    Math.pow(rgba1.green - rgba2.green, 2) +
    Math.pow(rgba1.blue - rgba2.blue, 2)
  )
}

/***** Color Names ************************************************************/

const namedCssColors = new Map<string, Rgb>([
  ['aliceblue', color_rgb(240, 248, 255)],
  ['antiquewhite', color_rgb(250, 235, 215)],
  ['aqua', color_rgb(0, 255, 255)],
  ['aquamarine', color_rgb(127, 255, 212)],
  ['azure', color_rgb(240, 255, 255)],
  ['beige', color_rgb(245, 245, 220)],
  ['bisque', color_rgb(255, 228, 196)],
  ['black', color_rgb(0, 0, 0)],
  ['blanchedalmond', color_rgb(255, 235, 205)],
  ['blue', color_rgb(0, 0, 255)],
  ['blueviolet', color_rgb(138, 43, 226)],
  ['brown', color_rgb(165, 42, 42)],
  ['burlywood', color_rgb(222, 184, 135)],
  ['cadetblue', color_rgb(95, 158, 160)],
  ['chartreuse', color_rgb(127, 255, 0)],
  ['chocolate', color_rgb(210, 105, 30)],
  ['coral', color_rgb(255, 127, 80)],
  ['cornflowerblue', color_rgb(100, 149, 237)],
  ['cornsilk', color_rgb(255, 248, 220)],
  ['crimson', color_rgb(220, 20, 60)],
  ['cyan', color_rgb(0, 255, 255)],
  ['darkblue', color_rgb(0, 0, 139)],
  ['darkcyan', color_rgb(0, 139, 139)],
  ['darkgoldenrod', color_rgb(184, 134, 11)],
  ['darkgray', color_rgb(169, 169, 169)],
  ['darkgreen', color_rgb(0, 100, 0)],
  ['darkkhaki', color_rgb(189, 183, 107)],
  ['darkmagenta', color_rgb(139, 0, 139)],
  ['darkolivegreen', color_rgb(85, 107, 47)],
  ['darkorange', color_rgb(255, 140, 0)],
  ['darkorchid', color_rgb(153, 50, 204)],
  ['darkred', color_rgb(139, 0, 0)],
  ['darksalmon', color_rgb(233, 150, 122)],
  ['darkseagreen', color_rgb(143, 188, 143)],
  ['darkslateblue', color_rgb(72, 61, 139)],
  ['darkslategray', color_rgb(47, 79, 79)],
  ['darkturquoise', color_rgb(0, 206, 209)],
  ['darkviolet', color_rgb(148, 0, 211)],
  ['deeppink', color_rgb(255, 20, 147)],
  ['deepskyblue', color_rgb(0, 191, 255)],
  ['dimgray', color_rgb(105, 105, 105)],
  ['dodgerblue', color_rgb(30, 144, 255)],
  ['firebrick', color_rgb(178, 34, 34)],
  ['floralwhite', color_rgb(255, 250, 240)],
  ['forestgreen', color_rgb(34, 139, 34)],
  ['fuchsia', color_rgb(255, 0, 255)],
  ['gainsboro', color_rgb(220, 220, 220)],
  ['ghostwhite', color_rgb(248, 248, 255)],
  ['gold', color_rgb(255, 215, 0)],
  ['goldenrod', color_rgb(218, 165, 32)],
  ['gray', color_rgb(128, 128, 128)],
  ['green', color_rgb(0, 128, 0)],
  ['greenyellow', color_rgb(173, 255, 47)],
  ['honeydew', color_rgb(240, 255, 240)],
  ['hotpink', color_rgb(255, 105, 180)],
  ['indianred', color_rgb(205, 92, 92)],
  ['indigo', color_rgb(75, 0, 130)],
  ['ivory', color_rgb(255, 255, 240)],
  ['khaki', color_rgb(240, 230, 140)],
  ['lavender', color_rgb(230, 230, 250)],
  ['lavenderblush', color_rgb(255, 240, 245)],
  ['lawngreen', color_rgb(124, 252, 0)],
  ['lemonchiffon', color_rgb(255, 250, 205)],
  ['lightblue', color_rgb(173, 216, 230)],
  ['lightcoral', color_rgb(240, 128, 128)],
  ['lightcyan', color_rgb(224, 255, 255)],
  ['lightgoldenrodyellow', color_rgb(250, 250, 210)],
  ['lightgray', color_rgb(211, 211, 211)],
  ['lightgreen', color_rgb(144, 238, 144)],
  ['lightpink', color_rgb(255, 182, 193)],
  ['lightsalmon', color_rgb(255, 160, 122)],
  ['lightseagreen', color_rgb(32, 178, 170)],
  ['lightskyblue', color_rgb(135, 206, 250)],
  ['lightslategray', color_rgb(119, 136, 153)],
  ['lightsteelblue', color_rgb(176, 196, 222)],
  ['lightyellow', color_rgb(255, 255, 224)],
  ['lime', color_rgb(0, 255, 0)],
  ['limegreen', color_rgb(50, 205, 50)],
  ['linen', color_rgb(250, 240, 230)],
  ['magenta', color_rgb(255, 0, 255)],
  ['maroon', color_rgb(128, 0, 0)],
  ['mediumaquamarine', color_rgb(102, 205, 170)],
  ['mediumblue', color_rgb(0, 0, 205)],
  ['mediumorchid', color_rgb(186, 85, 211)],
  ['mediumpurple', color_rgb(147, 112, 219)],
  ['mediumseagreen', color_rgb(60, 179, 113)],
  ['mediumslateblue', color_rgb(123, 104, 238)],
  ['mediumspringgreen', color_rgb(0, 250, 154)],
  ['mediumturquoise', color_rgb(72, 209, 204)],
  ['mediumvioletred', color_rgb(199, 21, 133)],
  ['midnightblue', color_rgb(25, 25, 112)],
  ['mintcream', color_rgb(245, 255, 250)],
  ['mistyrose', color_rgb(255, 228, 225)],
  ['moccasin', color_rgb(255, 228, 181)],
  ['navajowhite', color_rgb(255, 222, 173)],
  ['navy', color_rgb(0, 0, 128)],
  ['oldlace', color_rgb(253, 245, 230)],
  ['olive', color_rgb(128, 128, 0)],
  ['olivedrab', color_rgb(107, 142, 35)],
  ['orange', color_rgb(255, 165, 0)],
  ['orangered', color_rgb(255, 69, 0)],
  ['orchid', color_rgb(218, 112, 214)],
  ['palegoldenrod', color_rgb(238, 232, 170)],
  ['palegreen', color_rgb(152, 251, 152)],
  ['paleturquoise', color_rgb(175, 238, 238)],
  ['palevioletred', color_rgb(219, 112, 147)],
  ['papayawhip', color_rgb(255, 239, 213)],
  ['peachpuff', color_rgb(255, 218, 185)],
  ['peru', color_rgb(205, 133, 63)],
  ['pink', color_rgb(255, 192, 203)],
  ['plum', color_rgb(221, 160, 221)],
  ['powderblue', color_rgb(176, 224, 230)],
  ['purple', color_rgb(128, 0, 128)],
  ['rebeccapurple', color_rgb(102, 51, 153)],
  ['red', color_rgb(255, 0, 0)],
  ['rosybrown', color_rgb(188, 143, 143)],
  ['royalblue', color_rgb(65, 105, 225)],
  ['saddlebrown', color_rgb(139, 69, 19)],
  ['salmon', color_rgb(250, 128, 114)],
  ['sandybrown', color_rgb(244, 164, 96)],
  ['seagreen', color_rgb(46, 139, 87)],
  ['seashell', color_rgb(255, 245, 238)],
  ['sienna', color_rgb(160, 82, 45)],
  ['silver', color_rgb(192, 192, 192)],
  ['skyblue', color_rgb(135, 206, 235)],
  ['slateblue', color_rgb(106, 90, 205)],
  ['slategray', color_rgb(112, 128, 144)],
  ['snow', color_rgb(255, 250, 250)],
  ['springgreen', color_rgb(0, 255, 127)],
  ['steelblue', color_rgb(70, 130, 180)],
  ['tan', color_rgb(210, 180, 140)],
  ['teal', color_rgb(0, 128, 128)],
  ['thistle', color_rgb(216, 191, 216)],
  ['tomato', color_rgb(255, 99, 71)],
  ['turquoise', color_rgb(64, 224, 208)],
  ['violet', color_rgb(238, 130, 238)],
  ['wheat', color_rgb(245, 222, 179)],
  ['white', color_rgb(255, 255, 255)],
  ['whitesmoke', color_rgb(245, 245, 245)],
  ['yellow', color_rgb(255, 255, 0)],
  ['yellowgreen', color_rgb(154, 205, 50)]
])

export function color_isColorName(name: string): boolean {
  return namedCssColors.has(name.toLowerCase())
}

export function color_allColorNames(): L.List {
  return L.mkList(...Array.from(namedCssColors.keys()))
}

export function color_findColors(name: string): L.List {
  const results = []
  for (const [key, _value] of namedCssColors) {
    if (key.includes(name.toLowerCase())) {
      results.push(key)
    }
  }
  return L.mkList(...results)
}

/***** Color Strings **********************************************************/

// rgb-string?
// rgb-string->rgb

function fracToPercentString(n: number, m: number): string {
  return `${Math.trunc(n/m * 100)}%`
}

export function color_rgbToString (rgba: Rgb): string {
  return `rgb(${rgba.red}  ${rgba.green}  ${rgba.blue} / ${fracToPercentString(rgba.alpha, 255)})`
}

// color->string (variable type "color" argument to rgb string)

/***** RGB hex strings ********************************************************/

// component->hext
// rgb->hex
// color->hex
// hex->component
// hex->rgb

/***** HSV colors *************************************************************/

export interface Hsv extends L.Struct {
  [L.structKind]: 'hsv'
  hue: number,
  saturation: number,
  value: number,
  alpha: number
}

export function color_isHsv(v: L.Value): boolean {
  return L.isStructKind(v, 'hsv')
}

// hsv

export function color_hsv(...args: number[]): Hsv {
  if (args.length !== 3 && args.length !== 4) {
    throw new L.ScamperError('Runtime', `hsv: expects 3 or 4 arguments, but got ${args.length}`)
  }
  
  if (args[0] < 0 || args[0] > 360) {
    throw new L.ScamperError('Runtime', `hsv: expects hue to be in the an angle (0–360), but got ${args[0]}`)
  }
  const hue = args[0]

  if (args[1] < 0 || args[1] > 100) {
    throw new L.ScamperError('Runtime', `hsv: expects saturation to be a percentage (0–100), but got ${args[1]}`)
  }
  const saturation = args[1]

  if (args[2] < 0 || args[2] > 100) {
    throw new L.ScamperError('Runtime', `hsv: expects value to be a percentage (0–100), but got ${args[2]}`)
  }
  const value = args[2]

  if (args[3] !== undefined && (args[3] < 0 || args[3] > 255)) {
    throw new L.ScamperError('Runtime', `hsv: expects alpha to be in the range 0–255, but got ${args[3]}`)
  }
  const alpha = args[3] ?? 255
  return ({
    [L.scamperTag]: 'struct', [L.structKind]: 'hsv',
    hue, saturation, value, alpha
  })
}

export function color_hsvHue(hsv: Hsv): number {
  return hsv.hue
}

export function color_hsvSaturation(hsv: Hsv): number {
  return hsv.saturation
}

export function color_hsvValue(hsv: Hsv): number {
  return hsv.value
}

export function color_hsvAlpha(hsv: Hsv): number {
  return hsv.alpha
}

export function color_hsvComplement(h: Hsv): Hsv {
  return color_hsv((h.hue + 180) % 360, h.saturation, h.value, h.alpha)
}

// N.B., translated from the csc151 mediascheme implementation:
// https://github.com/grinnell-cs/csc151/blob/8dbcc594fbb5e3579e08ccc897c5fba7d973b779/colors.rkt#L379

export function color_rgbHue(r: Rgb): number {
  return rgbHueHelper(r.red, r.green, r.blue)
}

function rgbHueHelper(r: number, g: number, b: number): number {
  return rgbHueHelper2(Math.max(r, g, b), Math.min(r, g, b), r, g, b)
}

function rgbHueHelper2(max: number, min: number, r: number, g: number, b: number): number {
  if (max - min === 0) {
    return Math.random() * 360
  } else if (max === r) {
    return fixHue((g - b) / (max - min))
  } else if (max === g) {
    return fixHue(2 + (b - r) / (max - min))
  } else {
    return fixHue(4 + (r - g) / (max - min))
  }
}

function fixHue(h: number): number {
  return Math.round(60 * (h < 0 ? h + 6 : h))
}

export function color_rgbSaturation(r: Rgb): number {
  return rgbSaturationHelper(Math.min(r.red, r.green, r.blue),
                             Math.max(r.red, r.green, r.blue))
}

function rgbSaturationHelper(min: number, max: number): number {
  return max === 0 ? 0 : 100 * ((max - min) / max)
}

export function color_rgbValue(r: Rgb): number {
  return Math.round(100 * (Math.max(r.red, r.green, r.blue) / 255))
}

export function color_rgbToHsv(r: Rgb) {
  const ret = colorsys.rgbToHsv(r.red, r.green, r.blue)
  return color_hsv(ret.h, ret.s, ret.v, r.alpha)
}

export function color_hsvToString(hsv: Hsv): string {
  return `hsv(${hsv.hue} ${fracToPercentString(hsv.saturation, 100)}  ${fracToPercentString(hsv.value, 100)} / ${fracToPercentString(hsv.alpha, 255)})`
}

/***** Other predicates *******************************************************/

// color?

/***** Color conversion *******************************************************/

export function color_colorNameToRgb(name: string): Rgb {
  // Lower-cased, as `color_isColorName` lower-cases what it checks. It did not
  // before, so the guard accepted "RED", the lookup then missed it, and the
  // non-null assertion handed back an undefined typed as an Rgb (#154).
  const rgb = namedCssColors.get(name.toLowerCase())
  if (rgb === undefined) {
    throw new L.ScamperError('Runtime', `color-name->rgb: unknown color name ${name}`)
  }
  return rgb
}

// rgb->color-name
// color->rgb

export function color_hsvToRgb(hsv: Hsv): Rgb {
  const ret = colorsys.hsvToRgb(hsv.hue, hsv.saturation, hsv.value)
  return color_rgb(ret.r, ret.g, ret.b, hsv.alpha)
}

// color->color-name

/***** Color components *******************************************************/

// color-red
// color-green
// color-blue
// color-alpha

/***** Miscellaneous procedures ***********************************************/

// mod2
// color-equal?
// describe-color
// color->list

/***** Color transformations **************************************************/

export function color_rgbDarker(rgba: Rgb): Rgb {
  return color_rgb(
    Math.max(0, rgba.red - 16),
    Math.max(0, rgba.green - 16),
    Math.max(0, rgba.blue - 16),
    rgba.alpha
  )
}

export function color_rgbLighter(rgba: Rgb): Rgb {
  return color_rgb(
    Math.min(255, rgba.red + 16),
    Math.min(255, rgba.green + 16),
    Math.min(255, rgba.blue + 16),
    rgba.alpha
  )
}

export function color_rgbRedder(rgba: Rgb): Rgb {
  return color_rgb(
    Math.min(255, rgba.red + 32),
    Math.max(0, rgba.green - 16),
    Math.max(0, rgba.blue - 16),
    rgba.alpha
  )
}

export function color_rgbBluer(rgba: Rgb): Rgb {
  return color_rgb(
    Math.max(0, rgba.red - 16),
    Math.max(0, rgba.green - 16),
    Math.min(255, rgba.blue + 32),
    rgba.alpha
  )
}

export function color_rgbGreener(rgba: Rgb): Rgb {
  return color_rgb(
    Math.max(0, rgba.red - 16),
    Math.min(255, rgba.green + 32),
    Math.max(0, rgba.blue - 16),
    rgba.alpha
  )
}

export function color_rgbPseudoComplement(rgba: Rgb): Rgb {
  return color_rgb(
    255 - rgba.red,
    255 - rgba.green,
    255 - rgba.blue,
    rgba.alpha
  )
}

// rgb-complement

export function color_rgbGreyscale(rgba: Rgb): Rgb {
  const avg = 0.30 * rgba.red + 0.59 * rgba.green + 0.11 * rgba.blue
  return color_rgb(avg, avg, avg, rgba.alpha)
}

export function color_rgbPhaseshift(rgba: Rgb): Rgb {
  const shift = 128
  return color_rgb(
    (rgba.red + shift) % 256,
    (rgba.green + shift) % 256,
    (rgba.blue + shift) % 256,
    rgba.alpha
  )
}

export function color_rgbRotateComponents(rgba: Rgb): Rgb {
  return color_rgb(rgba.green, rgba.blue, rgba.red, rgba.alpha)
}

export function color_rgbThin(rgba: Rgb): Rgb {
  return color_rgb(
    rgba.red,
    rgba.green,
    rgba.blue,
    Math.max(0, rgba.alpha - 32)
  )
}

export function color_rgbThicken(rgba: Rgb): Rgb {
  return color_rgb(
    rgba.red,
    rgba.green,
    rgba.blue,
    Math.min(255, rgba.alpha + 32)
  )
}

/***** Color combinations *****************************************************/

export function color_rgbAdd(rgba1: Rgb, rgba2: Rgb): Rgb {
  return color_rgb(
    Math.min(255, rgba1.red + rgba2.red),
    Math.min(255, rgba1.green + rgba2.green),
    Math.min(255, rgba1.blue + rgba2.blue),
    rgba1.alpha
  )
}

export function color_rgbSubtract(rgba1: Rgb, rgba2: Rgb): Rgb {
  return color_rgb(
    Math.max(0, rgba1.red - rgba2.red),
    Math.max(0, rgba1.green - rgba2.green),
    Math.max(0, rgba1.blue - rgba2.blue),
    rgba1.alpha
  )
}

export function color_rgbAverage(rgba1: Rgb, rgba2: Rgb): Rgb {
  return color_rgb(
    (rgba1.red + rgba2.red) / 2,
    (rgba1.green + rgba2.green) / 2,
    (rgba1.blue + rgba2.blue) / 2,
    (rgba1.alpha + rgba2.alpha) / 2
  )
}

/***** Exports ****************************************************************/

// Generic colors

// RGB(A) colors

// Color names

// Color strings

// RGB hex strings

// HSV colors

// Other predicates

// Color conversion

// Color components

// Miscellaneous procedures

// Color transformations

// Color combinations
