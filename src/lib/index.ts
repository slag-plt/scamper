import { Library } from '../lpm'
import { imageLib } from './image.js'
import Lab from './lab.js'
import Music from './music.js'
import Test from './test.js'
import Audio from './audio.js'
import Canvas from './canvas.js'
import Html from './html.js'
import Reactive from './reactive.js'

export const builtinLibs: Map<string, Library> = new Map([
  ['image', imageLib],
  ['lab', Lab],
  ['music', Music],
  ['test', Test],
  ['audio', Audio],
  ['canvas', Canvas],
  ['html', Html],
  ['reactive', Reactive]
])

export { Prelude } from './prelude.js'
export { Runtime } from './runtime.js'
export default builtinLibs