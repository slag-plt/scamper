import { Value } from '../lang.js'
import { imageLib } from './image.js'
import Lab from './lab.js'
import Test from './test.js'

const builtinLibs: Map<string, [string, Value.T][]> = new Map([
  ['image', imageLib],
  ['lab', Lab],
  ['test', Test]
])

export default builtinLibs