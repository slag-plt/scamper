// These are coverage placeholders, not real tests. reactive.scm binds
// directly to the DOM event system, which needs a browser-API mocking
// strategy that is a separate, larger effort. Until that lands, this file
// just tracks which functions still need real tests.
import { describe, expect, test } from 'vitest'
import { runProgram } from './harness.js'

// Regression: button? is used in reactive.scm's on-button-click contract but
// defined in html.scm. reactive.scm must re-export it; otherwise on-button-click
// runs `(button? ...)` against an unbound name and throws "Variable not found"
// unless the user also imports html.
describe('button? resolves with only reactive imported', () => {
  test('button? is in scope', async () => {
    expect(await runProgram(`
    (import reactive)
    (button? 5)
    `)).toEqual(['#f'])
  })

  test('on-button-click contract fires cleanly on a non-button', async () => {
    const out = (await runProgram(`
    (import reactive)
    (on-button-click 42)
    `)).join('\n')
    expect(out).toContain('expected a button')
    expect(out).not.toContain('Variable not found')
  })
})

// Regression (#405): reactive-canvas and reactive-container are variadic in
// their subscriptions -- the implementations take `...subscriptions` and the
// documentation says "a number of subscriptions" -- but their declared
// signatures named a single `sub1` with no `&`, and the arity check is taken
// from the signature. Subscribing to more than one event therefore failed with
// "expected 6 arguments, got 8", which is what the reading in samples/ does
// (timer, mouse and keyboard at once) and what every reactive program of any
// size does. No timer here: a subscription that outlives the test would keep
// firing into a torn-down environment.
describe('a reactive component takes as many subscriptions as it is given', () => {
  const view = '(lambda (st canv) (canvas-rectangle! canv 0 0 10 10 "solid" "red"))'
  const update = '(lambda (msg st) st)'

  test('reactive-canvas accepts several', async () => {
    const out = (await runProgram(`
    (import canvas)
    (import reactive)
    (canvas? (reactive-canvas 10 10 0 ${view} ${update}
                              (on-mouse-click) (on-key-up) (on-mouse-hover)))
    `)).join('\n')
    expect(out).not.toContain('Arity mismatch')
    expect(out).toContain('#t')
  })

  test('reactive-canvas still accepts exactly one', async () => {
    const out = (await runProgram(`
    (import canvas)
    (import reactive)
    (canvas? (reactive-canvas 10 10 0 ${view} ${update} (on-mouse-click)))
    `)).join('\n')
    expect(out).toContain('#t')
  })

  test('reactive-container accepts several', async () => {
    const out = (await runProgram(`
    (import html)
    (import reactive)
    (reactive-container 0
      (lambda (st) (tag "p" (number->string st)))
      ${update}
      (on-mouse-click) (on-key-up))
    `)).join('\n')
    expect(out).not.toContain('Arity mismatch')
    expect(out).not.toContain('error')
  })
})

test.todo('html?')
test.todo('subscription?')
test.todo('reactive-canvas')
test.todo('reactive-container')
test.todo('on-mouse-click')
test.todo('on-mouse-hover')
test.todo('on-key-down')
test.todo('on-key-up')
test.todo('on-timer')
test.todo('on-note')
