import { Doc } from './api/docs.js'

import * as Audio from './api/audio.js'
import * as Prelude from './api/prelude.js'
import * as Image from './api/image.js'
import * as Lab from './api/lab.js'
import * as Music from './api/music.js'
import * as Test from './api/test.js'
import * as Canvas from './api/canvas.js'
import * as Html from './api/html.js'

import * as scamper from '../index.js'

const docLibs: Map<string, object> = new Map([
  ['prelude', Prelude as object],
  ['image', Image],
  ['lab', Lab],
  ['music', Music],
  ['test', Test],
  ['audio', Audio],
  ['canvas', Canvas],
  ['html', Html]
])

const scamperLibs = new Map<string, string[]>()
scamperLibs.set('prelude', scamper.Prelude.lib.map(([id, _]) => id))
for (const libName of scamper.builtinLibs.keys()) {
  scamperLibs.set(libName, scamper.builtinLibs.get(libName)!.lib.map(([id, _]) => id))
}

for (const name of scamperLibs.keys()) {
  console.log(`Checking ${name}...`)
  const docs = Object.values(docLibs.get(name)!) as Doc[]
  const documentedFns = docs.map(d => d.name) 
  const fns = scamperLibs.get(name)!
  fns.forEach(fn => {
    if (!documentedFns.includes(fn)) {
      console.log(`Missing ${fn}`)
    }
  })
}