import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import * as L from '../../lpm'

import * as colorsys from 'colorsys'

// NOTE: throughout the image library, we standardize on the `Rgb` struct as
//       the representation for colors. It is the responsibility of the various
//       data structures in the library to internally store colors as `Rgb`
//       values and accept multiple representations as input, converting via
//       `colorToRgb` as necessary.

/***** Generic Colors *********************************************************/

// N.B., color is a legacy function from the htdp library. Currently, we standardize
// on Rgb as the stored color type for shapes.
export function image_color(r: number, g: number, b: number, a: number): Rgb {
  checkContract(arguments, contract('color', [C.nonneg, C.nonneg, C.nonneg, C.nonneg]))
  return image_rgb(r, g, b, a)
}

/** Converts between various representations of color in Scamper. */
export function image_colorToRgb (v: any): Rgb {
  if (L.isStructKind(v, 'rgba')) {
    return v as Rgb
  } else if (typeof v === 'string') {
    return image_colorNameToRgb(v)
  } else if (L.isStructKind(v, 'hsv')) {
    return image_hsvToRgb(v as Hsv)
  } else {
    throw new L.ScamperError('Runtime', `Shapes expect a valid color, received a: ${L.typeOf(v)}`)
  }
}

export function image_colorQ (v: any): boolean {
  return (typeof v === 'string' && image_isColorName(v)) ||
    L.isStructKind(v, 'rgba') ||
    L.isStructKind(v, 'hsv')
}

export const image_colorS: C.Spec = {
  predicate: image_colorQ,
  errorMsg: (actual: any) => `expected a color, received ${L.typeOf(actual)}`
}

/***** RGB(A) Colors **********************************************************/

export interface Rgb extends L.Struct {
  [L.structKind]: 'rgba'
  red: number
  green: number
  blue: number
  alpha: number
}

export function image_isRgbComponent(n: number): boolean {
  checkContract(arguments, contract('rgb-component?', [C.number]))
  return n >= 0 && n <= 255
}

export function image_isRgb (v: any): boolean {
  checkContract(arguments, contract('rgb?', [C.any]))
  return L.isStructKind(v, 'rgba')
}

const rgbNumS: C.Spec = {
  predicate: (v: any) => typeof v === 'number' && image_isRgbComponent(v),
  errorMsg: (actual: any) => `expected a number in the range 0–255, received ${typeof actual === 'number' ? actual : L.typeOf(actual)}`
}

const rgbS: C.Spec = {
  predicate: image_isRgb,
  errorMsg: (actual: any) => `expected an RGB value, received ${L.typeOf(actual)}`
}

export function image_rgb(...args: number[]): Rgb {
  checkContract(arguments, contract('rgb', [], rgbNumS))
  if (args.length !== 3 && args.length !== 4) {
    throw new L.ScamperError('Runtime', `rgb: expects 3 or 4 arguments, but got ${args.length}`)
  }
  const red = Math.min(args[0], 255)
  const green = Math.min(args[1], 255)
  const blue = Math.min(args[2], 255)
  const alpha = args[3] ?? 255
  return ({
    [L.scamperTag]: 'struct', [L.structKind]: 'rgba',
    red, green, blue, alpha
  })
}

export function image_rgbRed(rgba: Rgb): number {
  checkContract(arguments, contract('rgb-red', [rgbS]))
  return rgba.red
}

export function image_rgbGreen(rgba: Rgb): number {
  checkContract(arguments, contract('rgb-green', [rgbS]))
  return rgba.green
}

export function image_rgbBlue(rgba: Rgb): number {
  checkContract(arguments, contract('rgb-blue', [rgbS]))
  return rgba.blue
}

export function image_rgbAlpha(rgba: Rgb): number {
  checkContract(arguments, contract('rgb-alpha', [rgbS]))
  return rgba.alpha
}

export function image_rgbDistance(rgba1: Rgb, rgba2: Rgb): number {
  checkContract(arguments, contract('rgb-distance', [rgbS, rgbS]))
  return Math.sqrt(
    Math.pow(rgba1.red - rgba2.red, 2) +
    Math.pow(rgba1.green - rgba2.green, 2) +
    Math.pow(rgba1.blue - rgba2.blue, 2)
  )
}

/***** Color Names ************************************************************/

const namedCssColors = new Map<string, Rgb>([
  ['aliceblue', image_rgb(240, 248, 255)],
  ['antiquewhite', image_rgb(250, 235, 215)],
  ['aqua', image_rgb(0, 255, 255)],
  ['aquamarine', image_rgb(127, 255, 212)],
  ['azure', image_rgb(240, 255, 255)],
  ['beige', image_rgb(245, 245, 220)],
  ['bisque', image_rgb(255, 228, 196)],
  ['black', image_rgb(0, 0, 0)],
  ['blanchedalmond', image_rgb(255, 235, 205)],
  ['blue', image_rgb(0, 0, 255)],
  ['blueviolet', image_rgb(138, 43, 226)],
  ['brown', image_rgb(165, 42, 42)],
  ['burlywood', image_rgb(222, 184, 135)],
  ['cadetblue', image_rgb(95, 158, 160)],
  ['chartreuse', image_rgb(127, 255, 0)],
  ['chocolate', image_rgb(210, 105, 30)],
  ['coral', image_rgb(255, 127, 80)],
  ['cornflowerblue', image_rgb(100, 149, 237)],
  ['cornsilk', image_rgb(255, 248, 220)],
  ['crimson', image_rgb(220, 20, 60)],
  ['cyan', image_rgb(0, 255, 255)],
  ['darkblue', image_rgb(0, 0, 139)],
  ['darkcyan', image_rgb(0, 139, 139)],
  ['darkgoldenrod', image_rgb(184, 134, 11)],
  ['darkgray', image_rgb(169, 169, 169)],
  ['darkgreen', image_rgb(0, 100, 0)],
  ['darkkhaki', image_rgb(189, 183, 107)],
  ['darkmagenta', image_rgb(139, 0, 139)],
  ['darkolivegreen', image_rgb(85, 107, 47)],
  ['darkorange', image_rgb(255, 140, 0)],
  ['darkorchid', image_rgb(153, 50, 204)],
  ['darkred', image_rgb(139, 0, 0)],
  ['darksalmon', image_rgb(233, 150, 122)],
  ['darkseagreen', image_rgb(143, 188, 143)],
  ['darkslateblue', image_rgb(72, 61, 139)],
  ['darkslategray', image_rgb(47, 79, 79)],
  ['darkturquoise', image_rgb(0, 206, 209)],
  ['darkviolet', image_rgb(148, 0, 211)],
  ['deeppink', image_rgb(255, 20, 147)],
  ['deepskyblue', image_rgb(0, 191, 255)],
  ['dimgray', image_rgb(105, 105, 105)],
  ['dodgerblue', image_rgb(30, 144, 255)],
  ['firebrick', image_rgb(178, 34, 34)],
  ['floralwhite', image_rgb(255, 250, 240)],
  ['forestgreen', image_rgb(34, 139, 34)],
  ['fuchsia', image_rgb(255, 0, 255)],
  ['gainsboro', image_rgb(220, 220, 220)],
  ['ghostwhite', image_rgb(248, 248, 255)],
  ['gold', image_rgb(255, 215, 0)],
  ['goldenrod', image_rgb(218, 165, 32)],
  ['gray', image_rgb(128, 128, 128)],
  ['green', image_rgb(0, 128, 0)],
  ['greenyellow', image_rgb(173, 255, 47)],
  ['honeydew', image_rgb(240, 255, 240)],
  ['hotpink', image_rgb(255, 105, 180)],
  ['indianred', image_rgb(205, 92, 92)],
  ['indigo', image_rgb(75, 0, 130)],
  ['ivory', image_rgb(255, 255, 240)],
  ['khaki', image_rgb(240, 230, 140)],
  ['lavender', image_rgb(230, 230, 250)],
  ['lavenderblush', image_rgb(255, 240, 245)],
  ['lawngreen', image_rgb(124, 252, 0)],
  ['lemonchiffon', image_rgb(255, 250, 205)],
  ['lightblue', image_rgb(173, 216, 230)],
  ['lightcoral', image_rgb(240, 128, 128)],
  ['lightcyan', image_rgb(224, 255, 255)],
  ['lightgoldenrodyellow', image_rgb(250, 250, 210)],
  ['lightgray', image_rgb(211, 211, 211)],
  ['lightgreen', image_rgb(144, 238, 144)],
  ['lightpink', image_rgb(255, 182, 193)],
  ['lightsalmon', image_rgb(255, 160, 122)],
  ['lightseagreen', image_rgb(32, 178, 170)],
  ['lightskyblue', image_rgb(135, 206, 250)],
  ['lightslategray', image_rgb(119, 136, 153)],
  ['lightsteelblue', image_rgb(176, 196, 222)],
  ['lightyellow', image_rgb(255, 255, 224)],
  ['lime', image_rgb(0, 255, 0)],
  ['limegreen', image_rgb(50, 205, 50)],
  ['linen', image_rgb(250, 240, 230)],
  ['magenta', image_rgb(255, 0, 255)],
  ['maroon', image_rgb(128, 0, 0)],
  ['mediumaquamarine', image_rgb(102, 205, 170)],
  ['mediumblue', image_rgb(0, 0, 205)],
  ['mediumorchid', image_rgb(186, 85, 211)],
  ['mediumpurple', image_rgb(147, 112, 219)],
  ['mediumseagreen', image_rgb(60, 179, 113)],
  ['mediumslateblue', image_rgb(123, 104, 238)],
  ['mediumspringgreen', image_rgb(0, 250, 154)],
  ['mediumturquoise', image_rgb(72, 209, 204)],
  ['mediumvioletred', image_rgb(199, 21, 133)],
  ['midnightblue', image_rgb(25, 25, 112)],
  ['mintcream', image_rgb(245, 255, 250)],
  ['mistyrose', image_rgb(255, 228, 225)],
  ['moccasin', image_rgb(255, 228, 181)],
  ['navajowhite', image_rgb(255, 222, 173)],
  ['navy', image_rgb(0, 0, 128)],
  ['oldlace', image_rgb(253, 245, 230)],
  ['olive', image_rgb(128, 128, 0)],
  ['olivedrab', image_rgb(107, 142, 35)],
  ['orange', image_rgb(255, 165, 0)],
  ['orangered', image_rgb(255, 69, 0)],
  ['orchid', image_rgb(218, 112, 214)],
  ['palegoldenrod', image_rgb(238, 232, 170)],
  ['palegreen', image_rgb(152, 251, 152)],
  ['paleturquoise', image_rgb(175, 238, 238)],
  ['palevioletred', image_rgb(219, 112, 147)],
  ['papayawhip', image_rgb(255, 239, 213)],
  ['peachpuff', image_rgb(255, 218, 185)],
  ['peru', image_rgb(205, 133, 63)],
  ['pink', image_rgb(255, 192, 203)],
  ['plum', image_rgb(221, 160, 221)],
  ['powderblue', image_rgb(176, 224, 230)],
  ['purple', image_rgb(128, 0, 128)],
  ['rebeccapurple', image_rgb(102, 51, 153)],
  ['red', image_rgb(255, 0, 0)],
  ['rosybrown', image_rgb(188, 143, 143)],
  ['royalblue', image_rgb(65, 105, 225)],
  ['saddlebrown', image_rgb(139, 69, 19)],
  ['salmon', image_rgb(250, 128, 114)],
  ['sandybrown', image_rgb(244, 164, 96)],
  ['seagreen', image_rgb(46, 139, 87)],
  ['seashell', image_rgb(255, 245, 238)],
  ['sienna', image_rgb(160, 82, 45)],
  ['silver', image_rgb(192, 192, 192)],
  ['skyblue', image_rgb(135, 206, 235)],
  ['slateblue', image_rgb(106, 90, 205)],
  ['slategray', image_rgb(112, 128, 144)],
  ['snow', image_rgb(255, 250, 250)],
  ['springgreen', image_rgb(0, 255, 127)],
  ['steelblue', image_rgb(70, 130, 180)],
  ['tan', image_rgb(210, 180, 140)],
  ['teal', image_rgb(0, 128, 128)],
  ['thistle', image_rgb(216, 191, 216)],
  ['tomato', image_rgb(255, 99, 71)],
  ['turquoise', image_rgb(64, 224, 208)],
  ['violet', image_rgb(238, 130, 238)],
  ['wheat', image_rgb(245, 222, 179)],
  ['white', image_rgb(255, 255, 255)],
  ['whitesmoke', image_rgb(245, 245, 245)],
  ['yellow', image_rgb(255, 255, 0)],
  ['yellowgreen', image_rgb(154, 205, 50)]
]);

export function image_isColorName(name: string): boolean {
  checkContract(arguments, contract('color-name?', [C.string]))
  return namedCssColors.has(name.toLowerCase())
}

export function image_allColorNames(): L.List {
  checkContract(arguments, contract('all-color-names', []))
  return L.mkList(...Array.from(namedCssColors.keys()))
}

export function image_findColors(name: string): L.List {
  checkContract(arguments, contract('find-colors', [C.string]))
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

export function image_rgbToString (rgba: Rgb): string {
  checkContract(arguments, contract('rgb->string', [rgbS]))
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

export function image_isHsv(v: any): boolean {
  return L.isStructKind(v, 'hsv')
}

const hsvS: C.Spec = {
  predicate: image_isHsv,
  errorMsg: (actual: any) => `expected an hsv value but received ${L.typeOf(actual)}`
}

// hsv

export function image_hsv(...args: number[]): Hsv {
  checkContract(arguments, contract('hsv', [], C.number))
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

export function image_hsvHue(hsv: Hsv): number {
  checkContract(arguments, contract('hsv-hue', [hsvS]))
  return hsv.hue
}

export function image_hsvSaturation(hsv: Hsv): number {
  checkContract(arguments, contract('hsv-saturation', [hsvS]))
  return hsv.saturation
}

export function image_hsvValue(hsv: Hsv): number {
  checkContract(arguments, contract('hsv-value', [hsvS]))
  return hsv.value
}

export function image_hsvAlpha(hsv: Hsv): number {
  checkContract(arguments, contract('hsv-alpha', [hsvS]))
  return hsv.alpha
}

export function image_hsvComplement(h: Hsv): Hsv {
  checkContract(arguments, contract('hsv-complement', [hsvS]))
  return image_hsv((h.hue + 180) % 360, h.saturation, h.value, h.alpha)
}

// N.B., translated from the csc151 mediascheme implementation:
// https://github.com/grinnell-cs/csc151/blob/8dbcc594fbb5e3579e08ccc897c5fba7d973b779/colors.rkt#L379

export function image_rgbHue(r: Rgb): number {
  checkContract(arguments, contract('rgb-hue', [rgbS]))
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

export function image_rgbSaturation(r: Rgb): number {
  checkContract(arguments, contract('rgb-saturation', [rgbS]))
  return rgbSaturationHelper(Math.max(r.red, r.green, r.blue),
                             Math.min(r.red, r.green, r.blue))
}

function rgbSaturationHelper(min: number, max: number): number {
  return max === 0 ? 0 : 100 * ((max - min) / max)
}

export function image_rgbValue(r: Rgb): number {
  checkContract(arguments, contract('rgb-value', [rgbS]))
  return Math.round(100 * (Math.max(r.red, r.green, r.blue) / 255))
}

export function image_rgbToHsv(r: Rgb) {
  checkContract(arguments, contract('rgb->hsv', [rgbS]))
  const ret = colorsys.rgbToHsv(r.red, r.green, r.blue)
  return image_hsv(ret.h, ret.s, ret.v, r.alpha)
}

export function image_hsvToString(hsv: Hsv): string {
  checkContract(arguments, contract('hsv->string', [hsvS]))
  return `hsv(${hsv.hue} ${fracToPercentString(hsv.saturation, 100)}  ${fracToPercentString(hsv.value, 100)} / ${fracToPercentString(hsv.alpha, 255)})`
}

/***** Other predicates *******************************************************/

// color?

/***** Color conversion *******************************************************/

export function image_colorNameToRgb(name: string): Rgb {
  checkContract(arguments, contract('color-name->rgb', [C.string]))
  if (!image_isColorName(name)) {
    throw new L.ScamperError('Runtime', `color-name->rgb: unknown color name ${name}`)
  }
  return namedCssColors.get(name)!
}

// rgb->color-name
// color->rgb

export function image_hsvToRgb(hsv: Hsv): Rgb {
  C.checkContract(arguments, contract('hsv->rgb', [hsvS]))
  const ret = colorsys.hsvToRgb(hsv.hue, hsv.saturation, hsv.value)
  return image_rgb(ret.r, ret.g, ret.b, hsv.alpha)
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

export function image_rgbDarker(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-darker', [rgbS]))
  return image_rgb(
    Math.max(0, rgba.red - 16),
    Math.max(0, rgba.green - 16),
    Math.max(0, rgba.blue - 16),
    rgba.alpha
  )
}

export function image_rgbLighter(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-lighter', [rgbS]))
  return image_rgb(
    Math.min(255, rgba.red + 16),
    Math.min(255, rgba.green + 16),
    Math.min(255, rgba.blue + 16),
    rgba.alpha
  )
}

export function image_rgbRedder(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-redder', [rgbS]))
  return image_rgb(
    Math.min(255, rgba.red + 32),
    Math.max(0, rgba.green - 16),
    Math.max(0, rgba.blue - 16),
    rgba.alpha
  )
}

export function image_rgbBluer(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-bluer', [rgbS]))
  return image_rgb(
    Math.max(0, rgba.red - 16),
    Math.max(0, rgba.green - 16),
    Math.min(255, rgba.blue + 32),
    rgba.alpha
  )
}

export function image_rgbGreener(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-greener', [rgbS]))
  return image_rgb(
    Math.max(0, rgba.red - 16),
    Math.min(255, rgba.green + 32),
    Math.max(0, rgba.blue - 16),
    rgba.alpha
  )
}

export function image_rgbPseudoComplement(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-pseudo-complement', [rgbS]))
  return image_rgb(
    255 - rgba.red,
    255 - rgba.green,
    255 - rgba.blue,
    rgba.alpha
  )
}

// rgb-complement

export function image_rgbGreyscale(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-greyscale', [rgbS]))
  const avg = (0.30 * rgba.red + 0.59 * rgba.green + 0.11 * rgba.blue) / 3
  return image_rgb(avg, avg, avg, rgba.alpha)
}

export function image_rgbPhaseshift(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-phaseshift', [rgbS]))
  const shift = 128
  return image_rgb(
    (rgba.red + shift) % 256,
    (rgba.green + shift) % 256,
    (rgba.blue + shift) % 256,
    rgba.alpha
  )
}

export function image_rgbRotateComponents(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-rotate-components', [rgbS]))
  return image_rgb(rgba.green, rgba.blue, rgba.red, rgba.alpha)
}

export function image_rgbThin(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-thin', [rgbS]))
  return image_rgb(
    rgba.red,
    rgba.green,
    rgba.blue,
    Math.min(0, rgba.alpha - 32)
  )
}

export function image_rgbThicken(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-thicken', [rgbS]))
  return image_rgb(
    rgba.red,
    rgba.green,
    rgba.blue,
    Math.min(255, rgba.alpha + 32)
  )
}

/***** Color combinations *****************************************************/

export function image_rgbAdd(rgba1: Rgb, rgba2: Rgb): Rgb {
  checkContract(arguments, contract('rgb-add', [rgbS, rgbS]))
  return image_rgb(
    Math.min(255, rgba1.red + rgba2.red),
    Math.min(255, rgba1.green + rgba2.green),
    Math.min(255, rgba1.blue + rgba2.blue),
    rgba1.alpha
  )
}

export function image_rgbSubtract(rgba1: Rgb, rgba2: Rgb): Rgb {
  checkContract(arguments, contract('rgb-subtract', [rgbS, rgbS]))
  return image_rgb(
    Math.max(0, rgba1.red - rgba2.red),
    Math.max(0, rgba1.green - rgba2.green),
    Math.max(0, rgba1.blue - rgba2.blue),
    rgba1.alpha
  )
}

export function image_rgbAverage(rgba1: Rgb, rgba2: Rgb): Rgb {
  checkContract(arguments, contract('rgb-average', [rgbS, rgbS]))
  return image_rgb(
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
