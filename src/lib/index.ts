import { Module } from "../lpm"
import { imageLib } from "./image/index.js"
import Lab from "./lab/index.js"
import Music from "./music/index.js"
import Test from "./test/index.js"
import Audio from "./audio/index.js"
import Canvas from "./canvas/index.js"
import Html from "./html/index.js"
import Reactive from "./reactive/index.js"
import Data from "./data"
import Rex from "./rex/index.js"
import Prelude from "./prelude/index.js"
import Runtime from "./runtime/index.js"
import Grader from "./grader/index.js"

// N.B., a library with a renderers/ folder also needs an entry in
// src/web/renderers.ts, or its custom Vue/HTML renderers never register in
// the browser -- this map and that file are two independent enumerations of
// the same library set.
export const builtinLibs = new Map<string, Module>([
  ["image", imageLib],
  ["lab", Lab],
  ["music", Music],
  ["test", Test],
  ["audio", Audio],
  ["canvas", Canvas],
  ["html", Html],
  ["reactive", Reactive],
  ["data", Data],
  ["rex", Rex],
  ["prelude", Prelude],
  ["runtime", Runtime],
  ["grader", Grader]
])

export {
  Prelude, Runtime,
}
export default builtinLibs
