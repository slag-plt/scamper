import { mount } from '@vue/test-utils'
import { describe, expect, test } from 'vitest'
import {
  Exp,
  expToString,
  mkApp,
  mkCond,
  mkId,
  mkLet,
  mkLit,
  mkMatch,
  mkPLit,
} from '../../src/scheme/ast.js'
import ExpRenderer from '../../src/scheme/ast-components/ExpRenderer.vue'
// Side-effect import: registers the Exp/Pat Vue renderers that ValueRenderer
// dispatches to (mirrors what src/app/web/renderers.ts wires up in the IDE).
import '../../src/scheme/renderers/vue.js'

// Regression for #318: in the interactive (web) trace, a `let` rendered via the
// shared HljsBindingForm component dropped the parentheses around its binding
// list and doubled the space after `let`, e.g. `(let  [next (- 5 1)] body)`
// instead of the correct `(let ([next (- 5 1)]) body)`. The canonical text form
// (expToString) was always correct; only the Vue rendering diverged. Assert the
// Vue rendering's text matches expToString for the binding forms.

/** The exact text the web renderer paints for expression `e`. */
function renderedText(e: Exp): string {
  return mount(ExpRenderer, { props: { value: e } }).element.textContent ?? ''
}

describe('binding forms render (web) identically to their text form (#318)', () => {
  test('let wraps its binding list in parens with no doubled space', () => {
    // (let ([next (- 5 1)]) (* 5 (factorial next)))
    const e = mkLet(
      [{ pat: mkId('next'), value: mkApp(mkId('-'), [mkLit(5), mkLit(1)]) }],
      mkApp(mkId('*'), [mkLit(5), mkApp(mkId('factorial'), [mkId('next')])]),
    )
    expect(renderedText(e)).toBe(expToString(e))
    expect(renderedText(e)).toBe('(let ([next (- 5 1)]) (* 5 (factorial next)))')
  })

  test('let with multiple bindings keeps them inside one paren group', () => {
    const e = mkLet(
      [
        { pat: mkId('x'), value: mkLit(2) },
        { pat: mkId('y'), value: mkLit(3) },
      ],
      mkApp(mkId('+'), [mkId('x'), mkId('y')]),
    )
    expect(renderedText(e)).toBe(expToString(e))
    expect(renderedText(e)).toBe('(let ([x 2] [y 3]) (+ x y))')
  })

  test('match still renders identically to its text form', () => {
    const e = mkMatch(mkId('n'), [
      { pat: mkPLit(0), body: mkLit(1) },
      { pat: mkId('k'), body: mkApp(mkId('*'), [mkId('k'), mkId('k')]) },
    ])
    expect(renderedText(e)).toBe(expToString(e))
  })

  test('cond still renders identically to its text form', () => {
    const e = mkCond([
      { test: mkApp(mkId('<'), [mkId('x'), mkLit(0)]), body: mkLit(-1) },
      { test: mkLit(true), body: mkLit(1) },
    ])
    expect(renderedText(e)).toBe(expToString(e))
  })
})
