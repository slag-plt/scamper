// @vitest-environment node
import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness'

// #508: a predicate written as `v instanceof <some DOM class>` throws outside
// the browser -- the CLI installs no DOM shim, so the identifier is simply not
// there and the call dies with `ReferenceError: HTMLCanvasElement is not
// defined`. A predicate that cannot answer "no" is worse than one that is
// absent: `(if (canvas? x) ... ...)` crashes on the branch that should have
// been the safe one. The answer outside the browser is `#f` -- there are no
// canvases there -- so each of these guards its global with `typeof`.
//
// N.B., this file overrides the suite's jsdom environment on purpose. Under
// jsdom the DOM globals *are* defined and this bug is invisible, so a test
// there would pass for the wrong reason. Node is the CLI's own condition.
describe('#508: DOM-class predicates where there is no DOM', () => {
  test('the DOM globals really are absent here', () => {
    expect(typeof HTMLCanvasElement).toBe('undefined')
    expect(typeof HTMLElement).toBe('undefined')
    expect(typeof AudioContext).toBe('undefined')
  })

  test('each one answers #f instead of throwing', async () => {
    const out = await runProgram(
      [
        '(import canvas)',
        '(import html)',
        '(import audio)',
        '(list (canvas? 5) (element? 5) (text-area? 5)',
        '      (button? 5) (context? 5) (audio-node? 5))',
      ].join('\n'),
    )
    expect(out).toEqual(['(list #f #f #f #f #f #f)'])
  })

  // The whole canvas surface goes the same way, since every contract declaring
  // `canvas?` calls it: `canvas-width` reported the ReferenceError rather than
  // its own contract violation.
  test('a contract declaring canvas? reports the contract violation', async () => {
    const out = await runProgram('(import image)\n(canvas-width 5)', {
      stripRanges: true,
    })
    expect(out).toEqual([
      'Runtime error: (error) expected a canvas, received number',
    ])
  })
})
