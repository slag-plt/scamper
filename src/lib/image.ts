import * as Drawing from './image/drawing.js'
import * as Rgb from './image/rgb.js'
import { Value } from '../lang.js'

export const imageLib: [string, Value.T][] =
  [
    ...Drawing.lib,
    ...Rgb.lib
  ]
