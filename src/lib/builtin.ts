import { imageLib } from './image.js'
import Lab from './lab.js'
import { Value } from '../value.js'

const builtinLibs: Map<string, [string, Value][]> = new Map([
  ['image', imageLib],
  ['lab', Lab]
])

export default builtinLibs