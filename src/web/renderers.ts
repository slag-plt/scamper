/**
 * Registers the Vue- and HTML-based custom renderers for every builtin
 * library's value types (drawings, colors, plots, test results, samples,
 * reactive files, compositions, ...), plus the AST/trace renderers used by
 * the IDE and tracer.
 *
 * Only ever load this in a browser context: it transitively imports Vue
 * single-file components, which the CLI's plain Node runtime cannot load.
 * src/scamper.ts loads it via a guarded dynamic import for exactly this
 * reason -- do not add a static import of this file anywhere reachable
 * from the CLI.
 */
import "../lib/image/renderers/html.js"
import "../lib/image/renderers/vue.js"
import "../lib/data/renderers/html.js"
import "../lib/data/renderers/vue.js"
import "../lib/prelude/renderers/html.js"
import "../lib/prelude/renderers/vue.js"
import "../lib/music/renderers/html.js"
import "../lib/music/renderers/vue.js"
import "../lib/audio/renderers/html.js"
import "../lib/audio/renderers/vue.js"
import "../lib/test/renderers/html.js"
import "../lib/test/renderers/vue.js"
import "../lpm/trace/renderers/html.js"
import "../lpm/trace/renderers/vue.js"
import "../scheme/renderers/vue.js"
