import * as L from "../lpm"
import { Fiber } from "../lpm/fiber.js"
import { builtinLibs } from "../lpm/builtin-registry.js"
import { SimpleErrorChannel } from "../lpm/output/simple-error.js"
import { compile } from "../scheme/index.js"

import audioSrc from "./audio.scm?raw"
import canvasSrc from "./canvas.scm?raw"
import dataSrc from "./data.scm?raw"
import htmlSrc from "./html.scm?raw"
import imageSrc from "./image.scm?raw"
import labSrc from "./lab.scm?raw"
import musicSrc from "./music.scm?raw"
import preludeSrc from "./prelude.scm?raw"
import reactiveSrc from "./reactive.scm?raw"
import rexSrc from "./rex.scm?raw"
import runtimeSrc from "./runtime.scm?raw"
import testSrc from "./test.scm?raw"

const librarySources: [string, string][] = [
  ["audio", audioSrc],
  ["canvas", canvasSrc],
  ["data", dataSrc],
  ["html", htmlSrc],
  ["image", imageSrc],
  ["lab", labSrc],
  ["music", musicSrc],
  ["prelude", preludeSrc],
  ["reactive", reactiveSrc],
  ["rex", rexSrc],
  ["runtime", runtimeSrc],
  ["test", testSrc],
]

/**
 * Compiles and runs a standard library module's Scamper source (a flat
 * sequence of `(define name (js-var "..."))` forms -- see src/lib/*.scm) and
 * snapshots the resulting top-level bindings as a Module.
 */
async function loadLibrary(name: string, src: string): Promise<L.Module> {
  const errChannel = new SimpleErrorChannel()
  const prog = await compile(errChannel, src)
  if (prog === undefined || errChannel.errors.length > 0) {
    throw new L.ICE(
      "lib.loadLibrary",
      `Failed to compile builtin library "${name}": ${errChannel.errors.map((e) => e.toString()).join("; ")}`,
    )
  }
  const fiber = new Fiber(prog)
  while (!fiber.isDone()) {
    fiber.step()
  }
  return fiber.topLevelEnv.getTopLevelAsModule()
}

// N.B., populates the shared registry (see builtin-registry.ts for why) in
// place, rather than building an independent Map, so that fiber.ts's
// loadModule() sees these too.
for (const [name, mod] of await Promise.all(
  librarySources.map(
    async ([name, src]): Promise<[string, L.Module]> => [
      name,
      await loadLibrary(name, src),
    ],
  ),
)) {
  builtinLibs.set(name, mod)
}

export { builtinLibs }
export default builtinLibs
