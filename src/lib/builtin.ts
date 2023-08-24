import { imageLib } from './image.js'
import { Value } from '../value.js'

const builtinLibs: Map<string, [string, Value][]> = new Map([
  ['image', imageLib]
])

export default builtinLibs