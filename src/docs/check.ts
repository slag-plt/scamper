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

const docLibs = new Map<string, object>([
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
scamperLibs.set('prelude', scamper.Prelude.lib.map(([id]) => id))
for (const libName of scamper.builtinLibs.keys()) {
  const lib = scamper.builtinLibs.get(libName)

  if (lib !== undefined) {
    scamperLibs.set(libName, lib.lib.map(([id]) => id))
  }
}

for (const name of scamperLibs.keys()) {
  console.log(`Checking ${name}...`)

  const docLib = docLibs.get(name)
  const fns = scamperLibs.get(name)

  if (docLib !== undefined && fns !== undefined) {
    const docs = Object.values(docLib) as Doc[]
    const documentedFns = docs.map(d => d.name)

    fns.forEach(fn => {
      if (!documentedFns.includes(fn)) {
        console.log(`Missing ${fn}`)
      }
    })
  }
}