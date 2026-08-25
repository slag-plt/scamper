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
await runEmbeds()
