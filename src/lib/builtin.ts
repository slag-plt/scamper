import { Value } from '../lang.js'
import { imageLib } from './image.js'
import Lab from './lab.js'

const builtinLibs: Map<string, [string, Value.T][]> = new Map([
  ['image', imageLib],
  ['lab', Lab]
])

export default builtinLibs