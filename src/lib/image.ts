import * as Drawing from './image/drawing.js'
import * as Font from './image/font.js'
import * as Img from './image/image.js'
import * as Rgb from './image/color.js'
import * as L from '../lpm'

export const imageLib: L.Library = new L.Library()

Drawing.lib.bindings.forEach((v, x) => { imageLib.registerValue(x, v) })
Font.lib.bindings.forEach((v, x) => { imageLib.registerValue(x, v) })
Img.lib.bindings.forEach((v, x) => { imageLib.registerValue(x, v) })
Rgb.lib.bindings.forEach((v, x) => { imageLib.registerValue(x, v) })