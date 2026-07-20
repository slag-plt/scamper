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
import "../js/image/renderers/html.js"
import "../js/image/renderers/vue.js"
import "../js/data/renderers/html.js"
import "../js/data/renderers/vue.js"
import "../js/prelude/renderers/html.js"
import "../js/prelude/renderers/vue.js"
import "../js/music/renderers/html.js"
import "../js/music/renderers/vue.js"
import "../js/audio/renderers/html.js"
import "../js/audio/renderers/vue.js"
import "../js/test/renderers/html.js"
import "../js/test/renderers/vue.js"
import "../lpm/trace/renderers/html.js"
import "../lpm/trace/renderers/vue.js"
import "../scheme/renderers/vue.js"
