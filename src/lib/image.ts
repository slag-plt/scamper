import * as Drawing from './image/drawing.js'
import * as Img from './image/image.js'
import * as Rgb from './image/rgb.js'
import { Value } from '../lang.js'

export const imageLib: [string, Value.T][] =
  [
    ...Drawing.lib,
    ...Img.lib,
    ...Rgb.lib
  ]
