import * as L from "../lpm"

// TODO: stub. Once the standard library is rewritten as Scamper code (using
// js-var to reach into src/js/), this map should hold those compiled
// modules, keyed by module name.
export const builtinLibs = new Map<string, L.Module>()

export default builtinLibs
