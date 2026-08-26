import { initialize } from '../../../scamper'
import { runEmbeds } from './embed'

/**
 * The reading widget's entry point (#375).
 *
 * A page includes this once; it finds every `.scamper-transcript` on the page
 * and replaces it with a transcript of its code and output. See
 * `docs/embedding.md`.
 */
await initialize()
// The custom renderers -- drawings, plots, compositions, test results -- are
// registered by a fire-and-forget dynamic import in scamper.ts, deliberately
// not awaited there (see its comment: awaiting it once broke run cancellation).
// The IDE gets away with that because a person has to press Run first. A page
// of widgets does not: runEmbeds() starts within a millisecond of load and
// loses the race, so every drawing in a reading renders as `(rectangle 60 30
// "solid" (rgba 0 128 128 255))` instead of a picture. Awaiting the same module
// here costs one import that is already in flight, and is what makes a reading
// full of images show images (#405).
await import('../renderers.js')
await runEmbeds()
