import * as Drawing from './image/drawing.js'
import * as Font from './image/font.js'
import * as Img from './image/image.js'
import * as Rgb from './image/color.js'
import * as R from '../lpm/runtime.js'

export const imageLib: R.Library = new R.Library()

imageLib.lib =[
  ...Drawing.lib.lib,
  ...Font.lib.lib,
  ...Img.lib.lib,
  ...Rgb.lib.lib
]
