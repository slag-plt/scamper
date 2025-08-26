import * as Drawing from './image/drawing.js'
import * as Font from './image/font.js'
import * as Img from './image/image.js'
import * as Rgb from './image/color.js'
import * as L from '../lpm'

export const imageLib: L.Library = new L.Library()

imageLib.lib =[
  ...Drawing.lib.lib,
  ...Font.lib.lib,
  ...Img.lib.lib,
  ...Rgb.lib.lib
]
