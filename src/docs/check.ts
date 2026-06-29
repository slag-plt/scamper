import { Doc } from "./api/docs.js"

import * as Audio from "./api/audio.js"
import * as Prelude from "./api/prelude.js"
import * as Image from "./api/image.js"
import * as Lab from "./api/lab.js"
import * as Music from "./api/music.js"
import * as Test from "./api/test.js"
import * as Canvas from "./api/canvas.js"
import * as Html from "./api/html.js"
import builtinLibs from "../lib/index.js"

const docLibs = new Map<string, object>([
  ["prelude", Prelude],
  ["image", Image],
  ["lab", Lab],
  ["music", Music],
  ["test", Test],
  ["audio", Audio],
  ["canvas", Canvas],
  ["html", Html],
])

const scamperLibs = new Map<string, string[]>()
for (const libName of builtinLibs.keys()) {
  scamperLibs.set(
    libName,
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    [...builtinLibs.get(libName)!.bindings.keys()]
  )
}

for (const name of scamperLibs.keys()) {
  console.log(`Checking ${name}...`)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const docs = Object.values(docLibs.get(name)!) as Doc[]
  const documentedFns = docs.map((d) => d.name)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const fns = scamperLibs.get(name)!
  fns.forEach((fn) => {
    if (!documentedFns.includes(fn)) {
      console.log(`Missing ${fn}`)
    }
  })
}
