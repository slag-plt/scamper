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
