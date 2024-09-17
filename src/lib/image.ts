import * as Drawing from './image/drawing.js'
import * as Img from './image/image.js'
import * as Rgb from './image/rgb.js'
import { Library, emptyLibrary, Value } from '../lang.js'

export const imageLib: Library = emptyLibrary()

imageLib.lib =[
  ...Drawing.lib.lib,
  ...Img.lib.lib,
  ...Rgb.lib.lib
]
