import { checkContract, contract } from '../../contract.js'
import * as C from '../../contract.js'
import * as Render from '../../display.js'
import { emptyLibrary, Library, registerValue, ScamperError, Value } from '../../lang.js'

import * as colorsys from 'colorsys'

// NOTE: throughout the image library, we standardize on the `Rgb` struct as
//       the representation for colors. It is the responsibility of the various
//       data structures in the library to internally store colors as `Rgb`
//       values and accept multiple representations as input, converting via
//       `colorToRgb` as necessary.

/***** Generic Colors *********************************************************/

// N.B., color is a legacy function from the htdp library. Currently, we standardize
// on Rgb as the stored color type for shapes.
function color (r: number, g: number, b: number, a: number): Rgb {
  checkContract(arguments, contract('color', [C.nonneg, C.nonneg, C.nonneg, C.nonneg]))
  return rgb(r, g, b, a)
}

/** Converts between various representations of color in Scamper. */
export function colorToRgb (v: any): Rgb {
  if (Value.isStructKind(v, 'rgba')) {
    return v
  } else if (typeof v === 'string') {
    return colorNameToRgb(v)
  } else if (Value.isStructKind(v, 'hsv')) {
    return hsvToRgb(v)
  } else {
    throw new ScamperError('Runtime', `Shapes expect a valid color, received a: ${Value.typeOf(v)}`)
  }
}

export function colorQ (v: any): boolean {
  return (typeof v === 'string' && isColorName(v)) ||
    Value.isStructKind(v, 'rgba') ||
    Value.isStructKind(v, 'hsv')
}

export const colorS: C.Spec = {
  predicate: colorQ,
  errorMsg: (actual: any) => `expected a color, received ${Value.typeOf(actual)}`
}

/***** RGB(A) Colors **********************************************************/

export interface Rgb extends Value.Struct {
  [Value.structKind]: 'rgba'
  red: number
  green: number
  blue: number
  alpha: number
}

function isRgbComponent (n: number): boolean {
  checkContract(arguments, contract('rgb-component?', [C.number]))
  return n >= 0 && n <= 255
}

export function isRgb (v: any): boolean {
  checkContract(arguments, contract('rgb?', [C.any]))
  return Value.isStructKind(v, 'rgba')
}

const rgbNumS: C.Spec = {
  predicate: (v: any) => typeof v === 'number' && isRgbComponent(v),
  errorMsg: (actual: any) => `expected a number in the range 0–255, received ${typeof actual === 'number' ? actual : Value.typeOf(actual)}`
}

const rgbS: C.Spec = {
  predicate: isRgb,
  errorMsg: (actual: any) => `expected an RGB value, received ${Value.typeOf(actual)}`
}

export function rgb(...args: number[]): Rgb {
  checkContract(arguments, contract('rgb', [], rgbNumS))
  if (args.length !== 3 && args.length !== 4) {
    throw new ScamperError('Runtime', `rgb: expects 3 or 4 arguments, but got ${args.length}`)
  }
  const red = Math.min(args[0], 255)
  const green = Math.min(args[1], 255)
  const blue = Math.min(args[2], 255)
  const alpha = args[3] ?? 255
  return ({
    [Value.scamperTag]: 'struct', [Value.structKind]: 'rgba',
    red, green, blue, alpha
  })
}

function rgbRed (rgba: Rgb): number {
  checkContract(arguments, contract('rgb-red', [rgbS]))
  return rgba.red
}

function rgbGreen (rgba: Rgb): number {
  checkContract(arguments, contract('rgb-green', [rgbS]))
  return rgba.green
}

function rgbBlue (rgba: Rgb): number {
  checkContract(arguments, contract('rgb-blue', [rgbS]))
  return rgba.blue
}

function rgbAlpha (rgba: Rgb): number {
  checkContract(arguments, contract('rgb-alpha', [rgbS]))
  return rgba.alpha
}

function rgbDistance (rgba1: Rgb, rgba2: Rgb): number {
  checkContract(arguments, contract('rgb-distance', [rgbS, rgbS]))
  return Math.sqrt(
    Math.pow(rgba1.red - rgba2.red, 2) +
    Math.pow(rgba1.green - rgba2.green, 2) +
    Math.pow(rgba1.blue - rgba2.blue, 2)
  )
}

/***** Color Names ************************************************************/

const namedCssColors: Map<string, Rgb> = new Map([
  ['aliceblue', rgb(240, 248, 255)],
  ['antiquewhite', rgb(250, 235, 215)],
  ['aqua', rgb(0, 255, 255)],
  ['aquamarine', rgb(127, 255, 212)],
  ['azure', rgb(240, 255, 255)],
  ['beige', rgb(245, 245, 220)],
  ['bisque', rgb(255, 228, 196)],
  ['black', rgb(0, 0, 0)],
  ['blanchedalmond', rgb(255, 235, 205)],
  ['blue', rgb(0, 0, 255)],
  ['blueviolet', rgb(138, 43, 226)],
  ['brown', rgb(165, 42, 42)],
  ['burlywood', rgb(222, 184, 135)],
  ['cadetblue', rgb(95, 158, 160)],
  ['chartreuse', rgb(127, 255, 0)],
  ['chocolate', rgb(210, 105, 30)],
  ['coral', rgb(255, 127, 80)],
  ['cornflowerblue', rgb(100, 149, 237)],
  ['cornsilk', rgb(255, 248, 220)],
  ['crimson', rgb(220, 20, 60)],
  ['cyan', rgb(0, 255, 255)],
  ['darkblue', rgb(0, 0, 139)],
  ['darkcyan', rgb(0, 139, 139)],
  ['darkgoldenrod', rgb(184, 134, 11)],
  ['darkgray', rgb(169, 169, 169)],
  ['darkgreen', rgb(0, 100, 0)],
  ['darkkhaki', rgb(189, 183, 107)],
  ['darkmagenta', rgb(139, 0, 139)],
  ['darkolivegreen', rgb(85, 107, 47)],
  ['darkorange', rgb(255, 140, 0)],
  ['darkorchid', rgb(153, 50, 204)],
  ['darkred', rgb(139, 0, 0)],
  ['darksalmon', rgb(233, 150, 122)],
  ['darkseagreen', rgb(143, 188, 143)],
  ['darkslateblue', rgb(72, 61, 139)],
  ['darkslategray', rgb(47, 79, 79)],
  ['darkturquoise', rgb(0, 206, 209)],
  ['darkviolet', rgb(148, 0, 211)],
  ['deeppink', rgb(255, 20, 147)],
  ['deepskyblue', rgb(0, 191, 255)],
  ['dimgray', rgb(105, 105, 105)],
  ['dodgerblue', rgb(30, 144, 255)],
  ['firebrick', rgb(178, 34, 34)],
  ['floralwhite', rgb(255, 250, 240)],
  ['forestgreen', rgb(34, 139, 34)],
  ['fuchsia', rgb(255, 0, 255)],
  ['gainsboro', rgb(220, 220, 220)],
  ['ghostwhite', rgb(248, 248, 255)],
  ['gold', rgb(255, 215, 0)],
  ['goldenrod', rgb(218, 165, 32)],
  ['gray', rgb(128, 128, 128)],
  ['green', rgb(0, 128, 0)],
  ['greenyellow', rgb(173, 255, 47)],
  ['honeydew', rgb(240, 255, 240)],
  ['hotpink', rgb(255, 105, 180)],
  ['indianred', rgb(205, 92, 92)],
  ['indigo', rgb(75, 0, 130)],
  ['ivory', rgb(255, 255, 240)],
  ['khaki', rgb(240, 230, 140)],
  ['lavender', rgb(230, 230, 250)],
  ['lavenderblush', rgb(255, 240, 245)],
  ['lawngreen', rgb(124, 252, 0)],
  ['lemonchiffon', rgb(255, 250, 205)],
  ['lightblue', rgb(173, 216, 230)],
  ['lightcoral', rgb(240, 128, 128)],
  ['lightcyan', rgb(224, 255, 255)],
  ['lightgoldenrodyellow', rgb(250, 250, 210)],
  ['lightgray', rgb(211, 211, 211)],
  ['lightgreen', rgb(144, 238, 144)],
  ['lightpink', rgb(255, 182, 193)],
  ['lightsalmon', rgb(255, 160, 122)],
  ['lightseagreen', rgb(32, 178, 170)],
  ['lightskyblue', rgb(135, 206, 250)],
  ['lightslategray', rgb(119, 136, 153)],
  ['lightsteelblue', rgb(176, 196, 222)],
  ['lightyellow', rgb(255, 255, 224)],
  ['lime', rgb(0, 255, 0)],
  ['limegreen', rgb(50, 205, 50)],
  ['linen', rgb(250, 240, 230)],
  ['magenta', rgb(255, 0, 255)],
  ['maroon', rgb(128, 0, 0)],
  ['mediumaquamarine', rgb(102, 205, 170)],
  ['mediumblue', rgb(0, 0, 205)],
  ['mediumorchid', rgb(186, 85, 211)],
  ['mediumpurple', rgb(147, 112, 219)],
  ['mediumseagreen', rgb(60, 179, 113)],
  ['mediumslateblue', rgb(123, 104, 238)],
  ['mediumspringgreen', rgb(0, 250, 154)],
  ['mediumturquoise', rgb(72, 209, 204)],
  ['mediumvioletred', rgb(199, 21, 133)],
  ['midnightblue', rgb(25, 25, 112)],
  ['mintcream', rgb(245, 255, 250)],
  ['mistyrose', rgb(255, 228, 225)],
  ['moccasin', rgb(255, 228, 181)],
  ['navajowhite', rgb(255, 222, 173)],
  ['navy', rgb(0, 0, 128)],
  ['oldlace', rgb(253, 245, 230)],
  ['olive', rgb(128, 128, 0)],
  ['olivedrab', rgb(107, 142, 35)],
  ['orange', rgb(255, 165, 0)],
  ['orangered', rgb(255, 69, 0)],
  ['orchid', rgb(218, 112, 214)],
  ['palegoldenrod', rgb(238, 232, 170)],
  ['palegreen', rgb(152, 251, 152)],
  ['paleturquoise', rgb(175, 238, 238)],
  ['palevioletred', rgb(219, 112, 147)],
  ['papayawhip', rgb(255, 239, 213)],
  ['peachpuff', rgb(255, 218, 185)],
  ['peru', rgb(205, 133, 63)],
  ['pink', rgb(255, 192, 203)],
  ['plum', rgb(221, 160, 221)],
  ['powderblue', rgb(176, 224, 230)],
  ['purple', rgb(128, 0, 128)],
  ['rebeccapurple', rgb(102, 51, 153)],
  ['red', rgb(255, 0, 0)],
  ['rosybrown', rgb(188, 143, 143)],
  ['royalblue', rgb(65, 105, 225)],
  ['saddlebrown', rgb(139, 69, 19)],
  ['salmon', rgb(250, 128, 114)],
  ['sandybrown', rgb(244, 164, 96)],
  ['seagreen', rgb(46, 139, 87)],
  ['seashell', rgb(255, 245, 238)],
  ['sienna', rgb(160, 82, 45)],
  ['silver', rgb(192, 192, 192)],
  ['skyblue', rgb(135, 206, 235)],
  ['slateblue', rgb(106, 90, 205)],
  ['slategray', rgb(112, 128, 144)],
  ['snow', rgb(255, 250, 250)],
  ['springgreen', rgb(0, 255, 127)],
  ['steelblue', rgb(70, 130, 180)],
  ['tan', rgb(210, 180, 140)],
  ['teal', rgb(0, 128, 128)],
  ['thistle', rgb(216, 191, 216)],
  ['tomato', rgb(255, 99, 71)],
  ['turquoise', rgb(64, 224, 208)],
  ['violet', rgb(238, 130, 238)],
  ['wheat', rgb(245, 222, 179)],
  ['white', rgb(255, 255, 255)],
  ['whitesmoke', rgb(245, 245, 245)],
  ['yellow', rgb(255, 255, 0)],
  ['yellowgreen', rgb(154, 205, 50)]
]);

function isColorName(name: string): boolean {
  checkContract(arguments, contract('color-name?', [C.string]))
  return namedCssColors.has(name.toLowerCase())
}

function allColorNames(): Value.List {
  checkContract(arguments, contract('all-color-names', []))
  return Value.mkList(...Array.from(namedCssColors.keys()))
}

function findColors (name: string): Value.List {
  checkContract(arguments, contract('find-colors', [C.string]))
  const results = []
  for (const [key, _value] of namedCssColors) {
    if (key.includes(name.toLowerCase())) {
      results.push(key)
    }
  }
  return Value.mkList(...results)
}

/***** Color Strings **********************************************************/

// rgb-string?
// rgb-string->rgb

function fracToPercentString(n: number, m: number): string {
  return `${Math.trunc(n/m * 100)}%`
}

export function rgbToString (rgba: Rgb): string {
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

interface Hsv extends Value.Struct {
  [Value.structKind]: 'hsv'
  hue: number,
  saturation: number,
  value: number,
  alpha: number
}

function isHsv(v: any): boolean {
  return Value.isStructKind(v, 'hsv')
}

const hsvS: C.Spec = {
  predicate: isHsv,
  errorMsg: (actual: any) => `expected an hsv value but received ${Value.typeOf(actual)}`
}

// hsv

function hsv(...args: number[]): Hsv {
  checkContract(arguments, contract('hsv', [], C.number))
  if (args.length !== 3 && args.length !== 4) {
    throw new ScamperError('Runtime', `hsv: expects 3 or 4 arguments, but got ${args.length}`)
  }
  
  if (args[0] < 0 || args[0] > 360) {
    throw new ScamperError('Runtime', `hsv: expects hue to be in the an angle (0–360), but got ${args[0]}`)
  }
  const hue = args[0]

  if (args[1] < 0 || args[1] > 100) {
    throw new ScamperError('Runtime', `hsv: expects saturation to be a percentage (0–100), but got ${args[1]}`)
  }
  const saturation = args[1]

  if (args[2] < 0 || args[2] > 100) {
    throw new ScamperError('Runtime', `hsv: expects value to be a percentage (0–100), but got ${args[2]}`)
  }
  const value = args[2]

  if (args[3] !== undefined && (args[3] < 0 || args[3] > 255)) {
    throw new ScamperError('Runtime', `hsv: expects alpha to be in the range 0–255, but got ${args[3]}`)
  }
  const alpha = args[3] ?? 255
  return ({
    [Value.scamperTag]: 'struct', [Value.structKind]: 'hsv',
    hue, saturation, value, alpha
  })
}

function hsvHue(hsv: Hsv): number {
  checkContract(arguments, contract('hsv-hue', [hsvS]))
  return hsv.hue
}

function hsvSaturation(hsv: Hsv): number {
  checkContract(arguments, contract('hsv-saturation', [hsvS]))
  return hsv.saturation
}

function hsvValue(hsv: Hsv): number {
  checkContract(arguments, contract('hsv-value', [hsvS]))
  return hsv.value
}

function hsvAlpha(hsv: Hsv): number {
  checkContract(arguments, contract('hsv-alpha', [hsvS]))
  return hsv.alpha
}

function hsvComplement(h: Hsv): Hsv {
  checkContract(arguments, contract('hsv-complement', [hsvS]))
  return hsv((h.hue + 180) % 360, h.saturation, h.value, h.alpha)
}

// N.B., translated from the csc151 mediascheme implementation:
// https://github.com/grinnell-cs/csc151/blob/8dbcc594fbb5e3579e08ccc897c5fba7d973b779/colors.rkt#L379

function rgbHue(r: Rgb): number {
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

function rgbSaturation(r: Rgb): number {
  checkContract(arguments, contract('rgb-saturation', [rgbS]))
  return rgbSaturationHelper(Math.max(r.red, r.green, r.blue),
                             Math.min(r.red, r.green, r.blue))
}

function rgbSaturationHelper(min: number, max: number): number {
  return max === 0 ? 0 : 100 * ((max - min) / max)
}

function rgbValue(r: Rgb): number {
  checkContract(arguments, contract('rgb-value', [rgbS]))
  return Math.round(100 * (Math.max(r.red, r.green, r.blue) / 255))
}

function rgbToHsv(r: Rgb) {
  checkContract(arguments, contract('rgb->hsv', [rgbS]))
  const ret = colorsys.rgbToHsv(r.red, r.green, r.blue)
  return hsv(ret.h, ret.s, ret.v, r.alpha)
}

function hsvToString(hsv: Hsv): string {
  checkContract(arguments, contract('hsv->string', [hsvS]))
  return `hsv(${hsv.hue} ${fracToPercentString(hsv.saturation, 100)}  ${fracToPercentString(hsv.value, 100)} / ${fracToPercentString(hsv.alpha, 255)})`
}

/***** Other predicates *******************************************************/

// color?

/***** Color conversion *******************************************************/

export function colorNameToRgb(name: string): Rgb {
  checkContract(arguments, contract('color-name->rgb', [C.string]))
  if (!isColorName(name)) {
    throw new ScamperError('Runtime', `color-name->rgb: unknown color name ${name}`)
  }
  return namedCssColors.get(name)!
}

// rgb->color-name
// color->rgb

export function hsvToRgb(hsv: Hsv): Rgb {
  C.checkContract(arguments, contract('hsv->rgb', [hsvS]))
  const ret = colorsys.hsvToRgb(hsv.hue, hsv.saturation, hsv.value)
  return rgb(ret.r, ret.g, ret.b, hsv.alpha)
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

function rgbDarker(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-darker', [rgbS]))
  return rgb(
    Math.max(0, rgba.red - 16),
    Math.max(0, rgba.green - 16),
    Math.max(0, rgba.blue - 16),
    rgba.alpha
  )
}

function rgbLighter(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-lighter', [rgbS]))
  return rgb(
    Math.min(255, rgba.red + 16),
    Math.min(255, rgba.green + 16),
    Math.min(255, rgba.blue + 16),
    rgba.alpha
  )
}

function rgbRedder(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-redder', [rgbS]))
  return rgb(
    Math.min(255, rgba.red + 32),
    Math.max(0, rgba.green - 16),
    Math.max(0, rgba.blue - 16),
    rgba.alpha
  )
}

function rgbBluer(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-bluer', [rgbS]))
  return rgb(
    Math.max(0, rgba.red - 16),
    Math.max(0, rgba.green - 16),
    Math.min(255, rgba.blue + 32),
    rgba.alpha
  )
}

function rgbGreener(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-greener', [rgbS]))
  return rgb(
    Math.max(0, rgba.red - 16),
    Math.min(255, rgba.green + 32),
    Math.max(0, rgba.blue - 16),
    rgba.alpha
  )
}

function rgbPseudoComplement(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-pseudo-complement', [rgbS]))
  return rgb(
    255 - rgba.red,
    255 - rgba.green,
    255 - rgba.blue,
    rgba.alpha
  )
}

// rgb-complement

function rgbGreyscale(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-greyscale', [rgbS]))
  const avg = (0.30 * rgba.red + 0.59 * rgba.green + 0.11 * rgba.blue) / 3
  return rgb(avg, avg, avg, rgba.alpha)
}

function rgbPhaseshift(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-phaseshift', [rgbS]))
  const shift = 128
  return rgb(
    (rgba.red + shift) % 256,
    (rgba.green + shift) % 256,
    (rgba.blue + shift) % 256,
    rgba.alpha
  )
}

function rgbRotateComponents(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-rotate-components', [rgbS]))
  return rgb(rgba.green, rgba.blue, rgba.red, rgba.alpha)
}

function rgbThin(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-thin', [rgbS]))
  return rgb(
    rgba.red,
    rgba.green,
    rgba.blue,
    Math.min(0, rgba.alpha - 32)
  )
}

function rgbThicken(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-thicken', [rgbS]))
  return rgb(
    rgba.red,
    rgba.green,
    rgba.blue,
    Math.min(255, rgba.alpha + 32)
  )
}

/***** Color combinations *****************************************************/

function rgbAdd(rgba1: Rgb, rgba2: Rgb): Rgb {
  checkContract(arguments, contract('rgb-add', [rgbS, rgbS]))
  return rgb(
    Math.min(255, rgba1.red + rgba2.red),
    Math.min(255, rgba1.green + rgba2.green),
    Math.min(255, rgba1.blue + rgba2.blue),
    rgba1.alpha
  )
}

function rgbSubtract(rgba1: Rgb, rgba2: Rgb): Rgb {
  checkContract(arguments, contract('rgb-subtract', [rgbS, rgbS]))
  return rgb(
    Math.max(0, rgba1.red - rgba2.red),
    Math.max(0, rgba1.green - rgba2.green),
    Math.max(0, rgba1.blue - rgba2.blue),
    rgba1.alpha
  )
}

export function rgbAverage(rgba1: Rgb, rgba2: Rgb): Rgb {
  checkContract(arguments, contract('rgb-average', [rgbS, rgbS]))
  return rgb(
    (rgba1.red + rgba2.red) / 2,
    (rgba1.green + rgba2.green) / 2,
    (rgba1.blue + rgba2.blue) / 2,
    (rgba1.alpha + rgba2.alpha) / 2
  )
}

/***** Rendering **************************************************************/

function renderRgb(rgb: Rgb): HTMLElement {
  const div = document.createElement('div')
  const textColor = rgbPseudoComplement(rgb)
  div.style.color = rgbToString(textColor)
  div.style.backgroundColor = rgbToString(rgb)
  div.style.width = 'fit-content'
  div.style.border = '1px solid black'
  div.style.padding = '0.25em'
  div.textContent = rgbToString(rgb)
  return div
}

Render.addCustomWebRenderer(isRgb, renderRgb)

function renderHsv(hsv: Hsv): HTMLElement {
  const div = document.createElement('div')
  const rgb = hsvToRgb(hsv)
  const textColor = rgbPseudoComplement(rgb)
  div.style.color = rgbToString(textColor)
  div.style.backgroundColor = rgbToString(rgb)
  div.style.width = 'fit-content'
  div.style.border = '1px solid black'
  div.style.padding = '0.25em'
  div.textContent = hsvToString(hsv)
  return div
}

Render.addCustomWebRenderer(isHsv, renderHsv)


/***** Exports ****************************************************************/

export const lib: Library = emptyLibrary()

// Generic colors
registerValue('color', color, lib)
registerValue('color?', colorQ, lib)

// RGB(A) colors
registerValue('rgb-component?', isRgbComponent, lib)
registerValue('rgb?', isRgb, lib)
registerValue('rgb', rgb, lib)
registerValue('rgb-red', rgbRed, lib)
registerValue('rgb-green', rgbGreen, lib)
registerValue('rgb-blue', rgbBlue, lib)
registerValue('rgb-alpha', rgbAlpha, lib)
registerValue('rgb-distance', rgbDistance, lib)

// Color names
registerValue('color-name?', isColorName, lib)
registerValue('all-color-names', allColorNames, lib)
registerValue('find-colors', findColors, lib)

// Color strings
registerValue('rgb->string', rgbToString, lib)

// RGB hex strings

// HSV colors
registerValue('hsv?', hsv, lib)
registerValue('hsv', hsv, lib)
registerValue('hsv-hue', hsvHue, lib)
registerValue('hsv-saturation', hsvSaturation, lib)
registerValue('hsv-value', hsvValue, lib)
registerValue('hsv-alpha', hsvAlpha, lib)
registerValue('hsv-complement', hsvComplement, lib)
registerValue('rgb-hue', rgbHue, lib)
registerValue('rgb-saturation', rgbSaturation, lib)
registerValue('rgb-value', rgbValue, lib)
registerValue('rgb->hsv', rgbToHsv, lib)
registerValue('hsv->string', hsvToString, lib)

// Other predicates

// Color conversion
registerValue('color-name->rgb', colorNameToRgb, lib)
registerValue('hsv->rgb', hsvToRgb, lib)

// Color components

// Miscellaneous procedures

// Color transformations
registerValue('rgb-darker', rgbDarker, lib)
registerValue('rgb-lighter', rgbLighter, lib)
registerValue('rgb-redder', rgbRedder, lib)
registerValue('rgb-bluer', rgbBluer, lib)
registerValue('rgb-greener', rgbGreener, lib)
registerValue('rgb-pseudo-complement', rgbPseudoComplement, lib)
registerValue('rgb-greyscale', rgbGreyscale, lib)
registerValue('rgb-phaseshift', rgbPhaseshift, lib)
registerValue('rgb-rotate-components', rgbRotateComponents, lib)
registerValue('rgb-thin', rgbThin, lib)
registerValue('rgb-thicken', rgbThicken, lib)

// Color combinations
registerValue('rgb-add', rgbAdd, lib)
registerValue('rgb-subtract', rgbSubtract, lib)
registerValue('rgb-average', rgbAverage, lib)
