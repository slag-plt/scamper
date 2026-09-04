import { initialize } from '../../../scamper'
import { runEmbeds } from './embed'

/**
 * The reading widget's entry point (#375).
 *
 * A page includes this once; it finds every `.scamper-transcript` on the page
 * and replaces it with a transcript of its code and output. See
 * `docs/embedding.md`.
 */
// initialize() also awaits scamper.ts's registration of the custom renderers
// -- drawings, plots, compositions, test results (#511). That matters here:
// runEmbeds() starts within a millisecond of load, and without those renderers
// every drawing in a reading comes out as `(rectangle 60 30 "solid" (rgba 0 128
// 128 255))` instead of a picture (#405).
await initialize()
await runEmbeds()
