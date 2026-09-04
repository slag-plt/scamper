import { expect, test } from 'vitest'
import { initialize } from '../../src/scamper'
import VueRenderer from '../../src/lpm/renderers/vue'
import { color_rgb } from '../../src/js/image/color'

// https://github.com/slag-plt/scamper/issues/511
//
// scamper.ts kicks off web/renderers.ts's renderer registration with a
// fire-and-forget `void import(...)`, so the chain below it is still fetching
// modules long after the importing test file's own tests have finished. When a
// worker's RPC closes with one of those fetches outstanding, vitest rejects it
// with an EnvironmentTeardownError; nothing awaits or catches the `void`ed
// promise, so it surfaces as an unhandled rejection and a run-level error --
// `npm run validate` reports `test: FAIL` with every test passing.
//
// The race is unreproducible on demand (~1 in 30 full-suite runs), so this
// pins the invariant that removes it instead of the timing that exposes it:
// initialize() must not return until the registration it started has finished.
// Deliberately does *not* import src/app/web/renderers -- doing so would
// register the renderers itself and the assertion would hold either way.
test('initialize() waits for the renderer registration it starts (#511)', async () => {
  await initialize()
  expect(VueRenderer.getCustomRendererFor(color_rgb(1, 2, 3))).not.toBeNull()
})
