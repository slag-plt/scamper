import * as Drawing from './drawing.js'
import * as Font from './font.js'
import * as Img from './image.js'
import * as Rgb from './color.js'
import * as L from '../../lpm'

export const imageLib: L.Module = new L.Module()

Drawing.lib.bindings.forEach((v, x) => { imageLib.registerValue(x, v) })
Font.lib.bindings.forEach((v, x) => { imageLib.registerValue(x, v) })
Img.lib.bindings.forEach((v, x) => { imageLib.registerValue(x, v) })
Rgb.lib.bindings.forEach((v, x) => { imageLib.registerValue(x, v) })
