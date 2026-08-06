import * as L from '../lpm'
import * as Audio from './audio/index.js'
import * as Canvas from './canvas/index.js'
import * as Data from './data/index.js'
// N.B., `FileLib`, not `File`: `File` is a DOM global.
import * as FileLib from './file/index.js'
import * as Html from './html/index.js'
import * as Image from './image/index.js'
import * as Lab from './lab/index.js'
import * as Music from './music/index.js'
import * as Prelude from './prelude/index.js'
import * as Reactive from './reactive/index.js'
import * as Rex from './rex/index.js'
import * as Runtime from './runtime/index.js'
import * as Test from './test/index.js'

// N.B., prelude/index.ts groups a handful of dynamically-named functions
// (char/string comparators) into plain records -- keyed by their
// already-prefixed binding names -- rather than individual named exports,
// since JS modules can't declare a dynamic number of top-level bindings. The
// container records themselves (prelude_charCompareFns, etc.) aren't bindings
// anyone should look up, so they're excluded below in favor of their
// flattened contents.
const {
  prelude_charCompareFns,
  prelude_charPredicateFns,
  prelude_stringCompareFns,
  ...preludeBindings
} = Prelude

// N.B., a library with a renderers/ folder also needs an entry in
// src/web/renderers.ts, or its custom Vue/HTML renderers never register in
// the browser -- this map and that file are two independent enumerations of
// the same library set.
//
// Every binding name below is prefixed so that flattening all modules into a
// single map can't collide. The prefix names the *concept*, which for most
// modules is the same as the module (`prelude_numberQ`, `canvas_makeCanvas`).
//
// N.B., `src/js/image/` is the exception: it backs the `image` Scheme library,
// which covers four concepts, so it exports `drawing_*`, `color_*`, `font_*`,
// and `image_*` (the last reserved for image *files*) -- see #103. A Scheme
// module drawing on several JS prefixes is already normal: image.scm binds
// `canvas?` to `canvas_canvasQ`.
const internals = new Map<string, L.Value>([
  ...Object.entries(Audio),
  ...Object.entries(Canvas),
  ...Object.entries(Data),
  ...Object.entries(FileLib),
  ...Object.entries(Html),
  ...Object.entries(Image),
  ...Object.entries(Lab),
  ...Object.entries(Music),
  ...Object.entries(preludeBindings),
  ...Object.entries(prelude_charCompareFns),
  ...Object.entries(prelude_charPredicateFns),
  ...Object.entries(prelude_stringCompareFns),
  ...Object.entries(Reactive),
  ...Object.entries(Rex),
  ...Object.entries(Runtime),
  ...Object.entries(Test),
])

/**
 * Looks up a value exported by the JS native package by its (prefixed)
 * binding name, e.g. `lookup("prelude_numberQ")`. Used by the `jsvar`
 * runtime op, which resolves Scamper code's references to raw JS values.
 */
export function lookup(name: string): L.Value {
  if (!internals.has(name)) {
    throw new L.ScamperError(
      'Runtime',
      `Attempted to look up "${name}" but it is not bound!`,
    )
  }
  return internals.get(name)
}

/**
 * The `js-var` primitive, exposed as an ordinary function: resolves a built-in
 * (JavaScript) binding by its prefixed internal name. Injected into every
 * library's load environment and exported explicitly by the loader (see
 * src/lib/index.ts), so `(js-var "prelude_numberQ")` is an ordinary
 * application, not a special form.
 */
export const jsVar = L.nameFn('js-var', (name: string) => lookup(name)) as L.JsFunction
