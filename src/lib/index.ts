import { Library } from '../lpm'
import { imageLib } from './image.js'
import Lab from './lab.js'
import Music from './music.js'
import Test from './test.js'
import Audio from './audio.js'
import Canvas from './canvas.js'
import Html from './html.js'
import Reactive from './reactive.js'
import Data from './data'
import Rex from './rex.js'
import Prelude from './prelude/index.js'
import Runtime from './runtime.js'

export const builtinLibs: Map<string, Library> = new Map([
  ['image', imageLib],
  ['lab', Lab],
  ['music', Music],
  ['test', Test],
  ['audio', Audio],
  ['canvas', Canvas],
  ['html', Html],
  ['reactive', Reactive],
  ['data', Data],
  ['rex', Rex],
  ['prelude', Prelude],
  ['runtime', Runtime]
])

async function initializeLibs () {
  for (const lib of builtinLibs.values()) {
    if (lib.initializer !== undefined) {
      await lib.initializer?.()
    }
  }
}

export { Prelude, Runtime, initializeLibs }
export default builtinLibs