import { Library } from '../lang.js'
import { imageLib } from './image.js'
import Lab from './lab.js'
import Music from './music.js'
import Test from './test.js'
import Audio from './audio.js'
import Canvas from './canvas.js'
import Html from './html.js'

export const builtinLibs: Map<string, Library> = new Map([
  ['image', imageLib],
  ['lab', Lab],
  ['music', Music],
  ['test', Test],
  ['audio', Audio],
  ['canvas', Canvas],
  ['html', Html]
])

export default builtinLibs