import { checkContract, contract } from '../../contract.js'
import * as C from '../../contract.js'
import * as Render from '../../display.js'
import { ScamperError, Value } from '../../lang.js'

/***** RGB(A) Colors **********************************************************/

interface Rgb extends Value.Struct {
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

function isRgb (v: any): boolean {
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

function rgb(...args: number[]): Rgb {
  checkContract(arguments, contract('rgb', [], rgbNumS))
  if (args.length !== 3 && args.length !== 4) {
    throw new ScamperError('Runtime', `rgb: expects 3 or 4 arguments, but got ${args.length}`)
  }
  const red = args[0]
  const green = args[1]
  const blue = args[2]
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
  ['AliceBlue', rgb(240, 248, 255)],
  ['AntiqueWhite', rgb(250, 235, 215)],
  ['Aqua', rgb(0, 255, 255)],
  ['Aquamarine', rgb(127, 255, 212)],
  ['Azure', rgb(240, 255, 255)],
  ['Beige', rgb(245, 245, 220)],
  ['Bisque', rgb(255, 228, 196)],
  ['Black', rgb(0, 0, 0)],
  ['BlanchedAlmond', rgb(255, 235, 205)],
  ['Blue', rgb(0, 0, 255)],
  ['BlueViolet', rgb(138, 43, 226)],
  ['Brown', rgb(165, 42, 42)],
  ['BurlyWood', rgb(222, 184, 135)],
  ['CadetBlue', rgb(95, 158, 160)],
  ['Chartreuse', rgb(127, 255, 0)],
  ['Chocolate', rgb(210, 105, 30)],
  ['Coral', rgb(255, 127, 80)],
  ['CornflowerBlue', rgb(100, 149, 237)],
  ['Cornsilk', rgb(255, 248, 220)],
  ['Crimson', rgb(220, 20, 60)],
  ['Cyan', rgb(0, 255, 255)],
  ['DarkBlue', rgb(0, 0, 139)],
  ['DarkCyan', rgb(0, 139, 139)],
  ['DarkGoldenRod', rgb(184, 134, 11)],
  ['DarkGray', rgb(169, 169, 169)],
  ['DarkGreen', rgb(0, 100, 0)],
  ['DarkKhaki', rgb(189, 183, 107)],
  ['DarkMagenta', rgb(139, 0, 139)],
  ['DarkOliveGreen', rgb(85, 107, 47)],
  ['DarkOrange', rgb(255, 140, 0)],
  ['DarkOrchid', rgb(153, 50, 204)],
  ['DarkRed', rgb(139, 0, 0)],
  ['DarkSalmon', rgb(233, 150, 122)],
  ['DarkSeaGreen', rgb(143, 188, 143)],
  ['DarkSlateBlue', rgb(72, 61, 139)],
  ['DarkSlateGray', rgb(47, 79, 79)],
  ['DarkTurquoise', rgb(0, 206, 209)],
  ['DarkViolet', rgb(148, 0, 211)],
  ['DeepPink', rgb(255, 20, 147)],
  ['DeepSkyBlue', rgb(0, 191, 255)],
  ['DimGray', rgb(105, 105, 105)],
  ['DodgerBlue', rgb(30, 144, 255)],
  ['FireBrick', rgb(178, 34, 34)],
  ['FloralWhite', rgb(255, 250, 240)],
  ['ForestGreen', rgb(34, 139, 34)],
  ['Fuchsia', rgb(255, 0, 255)],
  ['Gainsboro', rgb(220, 220, 220)],
  ['GhostWhite', rgb(248, 248, 255)],
  ['Gold', rgb(255, 215, 0)],
  ['GoldenRod', rgb(218, 165, 32)],
  ['Gray', rgb(128, 128, 128)],
  ['Green', rgb(0, 128, 0)],
  ['GreenYellow', rgb(173, 255, 47)],
  ['HoneyDew', rgb(240, 255, 240)],
  ['HotPink', rgb(255, 105, 180)],
  ['IndianRed', rgb(205, 92, 92)],
  ['Indigo', rgb(75, 0, 130)],
  ['Ivory', rgb(255, 255, 240)],
  ['Khaki', rgb(240, 230, 140)],
  ['Lavender', rgb(230, 230, 250)],
  ['LavenderBlush', rgb(255, 240, 245)],
  ['LawnGreen', rgb(124, 252, 0)],
  ['LemonChiffon', rgb(255, 250, 205)],
  ['LightBlue', rgb(173, 216, 230)],
  ['LightCoral', rgb(240, 128, 128)],
  ['LightCyan', rgb(224, 255, 255)],
  ['LightGoldenRodYellow', rgb(250, 250, 210)],
  ['LightGray', rgb(211, 211, 211)],
  ['LightGreen', rgb(144, 238, 144)],
  ['LightPink', rgb(255, 182, 193)],
  ['LightSalmon', rgb(255, 160, 122)],
  ['LightSeaGreen', rgb(32, 178, 170)],
  ['LightSkyBlue', rgb(135, 206, 250)],
  ['LightSlateGray', rgb(119, 136, 153)],
  ['LightSteelBlue', rgb(176, 196, 222)],
  ['LightYellow', rgb(255, 255, 224)],
  ['Lime', rgb(0, 255, 0)],
  ['LimeGreen', rgb(50, 205, 50)],
  ['Linen', rgb(250, 240, 230)],
  ['Magenta', rgb(255, 0, 255)],
  ['Maroon', rgb(128, 0, 0)],
  ['MediumAquaMarine', rgb(102, 205, 170)],
  ['MediumBlue', rgb(0, 0, 205)],
  ['MediumOrchid', rgb(186, 85, 211)],
  ['MediumPurple', rgb(147, 112, 219)],
  ['MediumSeaGreen', rgb(60, 179, 113)],
  ['MediumSlateBlue', rgb(123, 104, 238)],
  ['MediumSpringGreen', rgb(0, 250, 154)],
  ['MediumTurquoise', rgb(72, 209, 204)],
  ['MediumVioletRed', rgb(199, 21, 133)],
  ['MidnightBlue', rgb(25, 25, 112)],
  ['MintCream', rgb(245, 255, 250)],
  ['MistyRose', rgb(255, 228, 225)],
  ['Moccasin', rgb(255, 228, 181)],
  ['NavajoWhite', rgb(255, 222, 173)],
  ['Navy', rgb(0, 0, 128)],
  ['OldLace', rgb(253, 245, 230)],
  ['Olive', rgb(128, 128, 0)],
  ['OliveDrab', rgb(107, 142, 35)],
  ['Orange', rgb(255, 165, 0)],
  ['OrangeRed', rgb(255, 69, 0)],
  ['Orchid', rgb(218, 112, 214)],
  ['PaleGoldenRod', rgb(238, 232, 170)],
  ['PaleGreen', rgb(152, 251, 152)],
  ['PaleTurquoise', rgb(175, 238, 238)],
  ['PaleVioletRed', rgb(219, 112, 147)],
  ['PapayaWhip', rgb(255, 239, 213)],
  ['PeachPuff', rgb(255, 218, 185)],
  ['Peru', rgb(205, 133, 63)],
  ['Pink', rgb(255, 192, 203)],
  ['Plum', rgb(221, 160, 221)],
  ['PowderBlue', rgb(176, 224, 230)],
  ['Purple', rgb(128, 0, 128)],
  ['RebeccaPurple', rgb(102, 51, 153)],
  ['Red', rgb(255, 0, 0)],
  ['RosyBrown', rgb(188, 143, 143)],
  ['RoyalBlue', rgb(65, 105, 225)],
  ['SaddleBrown', rgb(139, 69, 19)],
  ['Salmon', rgb(250, 128, 114)],
  ['SandyBrown', rgb(244, 164, 96)],
  ['SeaGreen', rgb(46, 139, 87)],
  ['SeaShell', rgb(255, 245, 238)],
  ['Sienna', rgb(160, 82, 45)],
  ['Silver', rgb(192, 192, 192)],
  ['SkyBlue', rgb(135, 206, 235)],
  ['SlateBlue', rgb(106, 90, 205)],
  ['SlateGray', rgb(112, 128, 144)],
  ['Snow', rgb(255, 250, 250)],
  ['SpringGreen', rgb(0, 255, 127)],
  ['SteelBlue', rgb(70, 130, 180)],
  ['Tan', rgb(210, 180, 140)],
  ['Teal', rgb(0, 128, 128)],
  ['Thistle', rgb(216, 191, 216)],
  ['Tomato', rgb(255, 99, 71)],
  ['Turquoise', rgb(64, 224, 208)],
  ['Violet', rgb(238, 130, 238)],
  ['Wheat', rgb(245, 222, 179)],
  ['White', rgb(255, 255, 255)],
  ['WhiteSmoke', rgb(245, 245, 245)],
  ['Yellow', rgb(255, 255, 0)],
  ['YellowGreen', rgb(154, 205, 50)]
]);

function isColorName(name: string): boolean {
  checkContract(arguments, contract('color-name?', [C.string]))
  return namedCssColors.has(name)
}

function allColorNames(): Value.List {
  checkContract(arguments, contract('all-color-names', []))
  return Value.mkList(...Array.from(namedCssColors.keys()))
}

function findColors (name: string): Value.List {
  checkContract(arguments, contract('find-colors', [C.string]))
  const results = []
  for (const [key, value] of namedCssColors) {
    if (key.includes(name)) {
      results.push(value)
    }
  }
  return Value.mkList(...results)
}

/***** Color Strings **********************************************************/

// rgb-string?
// rgb-string->rgb

function rgbToString (rgba: Rgb): string {
  checkContract(arguments, contract('rgb-to-string', [rgbS]))
  return `rgb(${rgba.red}, ${rgba.green}, ${rgba.blue}, ${rgba.alpha})`
}

function fracToPercentString(n: number, m: number): string {
  return `${Math.trunc(n/m * 100)}%`
}

function rbgToCssString (rgba: Rgb): string {
  checkContract(arguments, contract('rgb-to-css-string', [rgbS]))
  return `rgb(${rgba.red} ${rgba.green} ${rgba.blue} / ${fracToPercentString(rgba.alpha, 255)})`
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
  errorMsg: (actual: any) => `expected a number in the range 0–255, received ${typeof actual === 'number' ? actual : Value.typeOf(actual)}`
}

// hsv

function hsv(...args: number[]): Hsv {
  checkContract(arguments, contract('hsv', [], rgbNumS))
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

// rgb->hue
// rgb->saturation
// rgb->value
// rhb->hsv

/***** Other predicates *******************************************************/

// color?

/***** Color conversion *******************************************************/

function colorNameToRgb(name: string): Rgb {
  checkContract(arguments, contract('color-name->rgb', [C.string]))
  if (!isColorName(name)) {
    throw new ScamperError('Runtime', `color-name->rgb: unknown color name ${name}`)
  }
  return namedCssColors.get(name)!
}

// rgb->color-name
// color->rgb
// hsv->rgb
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
    Math.min(255, rgba.red + 16),
    rgba.green,
    rgba.blue,
    rgba.alpha
  )
}

function rgbBluer(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-bluer', [rgbS]))
  return rgb(
    rgba.red,
    rgba.green,
    Math.min(255, rgba.blue + 16),
    rgba.alpha
  )
}

function rgbGreener(rgba: Rgb): Rgb {
  checkContract(arguments, contract('rgb-greener', [rgbS]))
  return rgb(
    rgba.red,
    Math.min(255, rgba.green + 16),
    rgba.blue,
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
    Math.min(255, rgba1.alpha + rgba2.alpha)
  )
}

function rgbSubtract(rgba1: Rgb, rgba2: Rgb): Rgb {
  checkContract(arguments, contract('rgb-subtract', [rgbS, rgbS]))
  return rgb(
    Math.max(0, rgba1.red - rgba2.red),
    Math.max(0, rgba1.green - rgba2.green),
    Math.max(0, rgba1.blue - rgba2.blue),
    Math.max(0, rgba1.alpha - rgba2.alpha)
  )
}

function rgbAverage(rgba1: Rgb, rgba2: Rgb): Rgb {
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
  div.style.color = rbgToCssString(textColor)
  div.style.backgroundColor = rbgToCssString(rgb)
  div.style.width = 'fit-content'
  div.style.border = '1px solid black'
  div.style.padding = '0.25em'
  div.textContent = rgbToString(rgb)
  return div
}

Render.addCustomWebRenderer(isRgb, renderRgb)

/***** Exports ****************************************************************/

export const lib: [string, Value.T][] = []

function registerFn (name: string, fn: Function): void {
  Value.nameFn(name, fn)
  lib.push([name, fn])
}

// RGB(A) colors
registerFn('rgb-component?', isRgbComponent)
registerFn('rgb?', isRgb)
registerFn('rgb', rgb)
registerFn('rgb-red', rgbRed)
registerFn('rgb-green', rgbGreen)
registerFn('rgb-blue', rgbBlue)
registerFn('rgb-alpha', rgbAlpha)
registerFn('rgb-distance', rgbDistance)

// Color names
registerFn('color-name?', isColorName)
registerFn('all-color-names', allColorNames)
registerFn('find-colors', findColors)

// Color strings
registerFn('rbg->string', rgbToString)

// RGB hex strings

// HSV colors
registerFn('hsv?', hsv)
registerFn('hsv', hsv)
registerFn('hsv-hue', hsvHue)
registerFn('hsv-saturation', hsvSaturation)
registerFn('hsv-value', hsvValue)
registerFn('hsv-alpha', hsvAlpha)
registerFn('hsv-complement', hsvComplement)

// Other predicates

// Color conversion
registerFn('color-name->rgb', colorNameToRgb)

// Color components

// Miscellaneous procedures

// Color transformations
registerFn('rgb-darker', rgbDarker)
registerFn('rgb-lighter', rgbLighter)
registerFn('rgb-redder', rgbRedder)
registerFn('rgb-bluer', rgbBluer)
registerFn('rgb-greener', rgbGreener)
registerFn('rgb-pseudo-complement', rgbPseudoComplement)
registerFn('rgb-greyscale', rgbGreyscale)
registerFn('rgb-phaseshift', rgbPhaseshift)
registerFn('rgb-rotate-components', rgbRotateComponents)
registerFn('rgb-thin', rgbThin)
registerFn('rgb-thicken', rgbThicken)

// Color combinations
registerFn('rgb-add', rgbAdd)
registerFn('rgb-subtract', rgbSubtract)
registerFn('rgb-average', rgbAverage)
