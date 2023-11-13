import { Value } from '../lang.js'
import { imageLib } from './image.js'
import Lab from './lab.js'
import Music from './music.js'
import Test from './test.js'
import Audio from './audio.js'

const builtinLibs: Map<string, [string, Value.T][]> = new Map([
  ['image', imageLib],
  ['lab', Lab],
  ['music', Music],
  ['test', Test],
  ['audio', Audio],
])

export default builtinLibs