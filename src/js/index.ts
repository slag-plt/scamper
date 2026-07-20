import * as L from "../lpm"
import * as Audio from "./audio/index.js"
import * as Canvas from "./canvas/index.js"
import * as Data from "./data/index.js"
import * as Html from "./html/index.js"
import * as Image from "./image/index.js"
import * as Lab from "./lab/index.js"
import * as Music from "./music/index.js"
import * as Prelude from "./prelude/index.js"
import * as Reactive from "./reactive/index.js"
import * as Rex from "./rex/index.js"
import * as Runtime from "./runtime/index.js"
import * as Test from "./test/index.js"

// N.B., prelude/index.ts groups a handful of dynamically-named functions
// (list accessors, char/string comparators) into plain records rather than
// individual named exports, since JS modules can't declare a dynamic number
// of top-level bindings. Flatten those into the prelude namespace here so
// they're reachable like any other export.
const preludeNamespace: Record<string, L.Value> = {
  ...Prelude,
  ...Prelude.listAccessorFns,
  ...Prelude.charCompareFns,
  ...Prelude.charPredicateFns,
  ...Prelude.stringCompareFns,
}

// N.B., a library with a renderers/ folder also needs an entry in
// src/web/renderers.ts, or its custom Vue/HTML renderers never register in
// the browser -- this map and that file are two independent enumerations of
// the same library set.
const internals = new Map<string, Map<string, L.Value>>([
  ["audio", new Map(Object.entries(Audio))],
  ["canvas", new Map(Object.entries(Canvas))],
  ["data", new Map(Object.entries(Data))],
  ["html", new Map(Object.entries(Html))],
  ["image", new Map(Object.entries(Image))],
  ["lab", new Map(Object.entries(Lab))],
  ["music", new Map(Object.entries(Music))],
  ["prelude", new Map(Object.entries(preludeNamespace))],
  ["reactive", new Map(Object.entries(Reactive))],
  ["rex", new Map(Object.entries(Rex))],
  ["runtime", new Map(Object.entries(Runtime))],
  ["test", new Map(Object.entries(Test))],
])

/**
 * Looks up a value exported by one of the standard library modules by its
 * (JS) export name, e.g. `lookup("prelude", "numberQ")`. Used by the `jsvar`
 * runtime op, which resolves Scamper code's references to raw JS values.
 */
export function lookup(moduleName: string, varName: string): L.Value {
  const mod = internals.get(moduleName)
  if (!mod) {
    throw new L.ScamperError(
      "Runtime",
      `Attempted to look up module "${moduleName}" but it does not exist!`,
    )
  }
  if (!mod.has(varName)) {
    throw new L.ScamperError(
      "Runtime",
      `Attempted to look up "${varName}" in module "${moduleName}" but it is not bound!`,
    )
  }
  return mod.get(varName)
}
