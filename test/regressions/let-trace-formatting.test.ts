import { mount } from '@vue/test-utils'
import { describe, expect, test } from 'vitest'
import {
  Exp,
  expToLayout,
  layoutToString,
  mkApp,
  mkCond,
  mkId,
  mkLam,
  mkLet,
  mkLit,
  mkMatch,
  mkPLit,
} from '../../src/scheme/ast.js'
import ExpRenderer from '../../src/scheme/ast-components/ExpRenderer.vue'
// Side-effect import: registers the Exp/Pat Vue renderers that ValueRenderer
// dispatches to (mirrors what src/app/web/renderers.ts wires up in the IDE).
import '../../src/scheme/renderers/vue.js'

// Regression for #318 and the follow-on renderer unification: the web trace and
// the text form must render every surface form identically. Both derive from a
// single Layout (src/scheme/ast.ts) and lay it out from a single plan
// (src/scheme/pretty.ts) -- text via layoutToString, web via LayoutRenderer --
// so parity holds by construction rather than by hand-syncing two components.
// (#318 originally: a `let` rendered via the old HljsBindingForm dropped its
// binding-list parens and doubled the space after `let`, e.g.
// `(let  [next (- 5 1)] body)`.)
//
// The comparison is against `layoutToString`, not `expToString`: the latter is
// deliberately flat, being what goes into an error message (see ast.ts).

/** What the text backend draws for `e`, at the default format mode. */
function laidOut(e: Exp): string {
  return layoutToString(expToLayout(e))
}

/**
 * `node`'s text, counting a `<br>` as the line break it draws.
 *
 * `textContent` alone would not do: the web backend draws a break as a `<br>`,
 * which contributes no text of its own, so every break would silently vanish
 * and the comparison would pass on output that does not match.
 */
function textOf(node: Node): string {
  if (node.nodeType === Node.TEXT_NODE) return node.textContent ?? ''
  if (node instanceof HTMLBRElement) return '\n'
  return Array.from(node.childNodes).map(textOf).join('')
}

/** The exact text the web renderer paints for `e`. */
function renderedText(e: Exp): string {
  return textOf(mount(ExpRenderer, { props: { value: e } }).element as Node)
}

describe('binding forms render (web) identically to their text form (#318)', () => {
  test('let wraps its binding list in parens with no doubled space', () => {
    // (let ([next (- 5 1)]) (* 5 (factorial next)))
    const e = mkLet(
      [{ pat: mkId('next'), value: mkApp(mkId('-'), [mkLit(5), mkLit(1)]) }],
      mkApp(mkId('*'), [mkLit(5), mkApp(mkId('factorial'), [mkId('next')])]),
    )
    expect(renderedText(e)).toBe(laidOut(e))
    expect(renderedText(e)).toBe(
      '(let ([next (- 5 1)])\n  (* 5 (factorial next)))',
    )
  })

  test('let with multiple bindings keeps them inside one paren group', () => {
    const e = mkLet(
      [
        { pat: mkId('x'), value: mkLit(2) },
        { pat: mkId('y'), value: mkLit(3) },
      ],
      mkApp(mkId('+'), [mkId('x'), mkId('y')]),
    )
    expect(renderedText(e)).toBe(laidOut(e))
    expect(renderedText(e)).toBe('(let ([x 2]\n      [y 3])\n  (+ x y))')
  })

  test('match still renders identically to its text form', () => {
    const e = mkMatch(mkId('n'), [
      { pat: mkPLit(0), body: mkLit(1) },
      { pat: mkId('k'), body: mkApp(mkId('*'), [mkId('k'), mkId('k')]) },
    ])
    expect(renderedText(e)).toBe(laidOut(e))
  })

  test('cond still renders identically to its text form', () => {
    const e = mkCond([
      { test: mkApp(mkId('<'), [mkId('x'), mkLit(0)]), body: mkLit(-1) },
      { test: mkLit(true), body: mkLit(1) },
    ])
    expect(renderedText(e)).toBe(laidOut(e))
  })

  test('lambda parenthesizes its parameter list (web matches text)', () => {
    // The old web renderer dropped the param parens, e.g. `(lambda x y ...)`.
    const e = mkLam([mkId('x'), mkId('y')], mkApp(mkId('+'), [mkId('x'), mkId('y')]))
    expect(renderedText(e)).toBe(laidOut(e))
    expect(renderedText(e)).toBe('(lambda (x y)\n  (+ x y))')
  })

  test('null renders as null, not () (web matches text)', () => {
    // A null (empty-list) value substituted into a trace renders as `null`, the
    // same token the text renderer uses -- not `()`, which the web value
    // renderer used to emit.
    const e = mkLit(null)
    expect(renderedText(e)).toBe(laidOut(e))
    expect(renderedText(e)).toBe('null')
  })
})

// The web renderer tags each token's syntactic role with the scamper-hl-*
// classes the highlight stylesheet themes, straight from the AST -- no hljs
// tokenizer run. Highlighting must never change the rendered text, only classes.
describe('syntax highlighting is emitted directly on the AST HTML', () => {
  function mountExp(e: Exp): HTMLElement {
    return mount(ExpRenderer, { props: { value: e } }).element as HTMLElement
  }

  test('special-form keywords get the keyword class', () => {
    const e = mkLet([{ pat: mkId('x'), value: mkLit(1) }], mkId('x'))
    const el = mountExp(e)
    expect(el.querySelector('.scamper-hl-keyword')?.textContent).toBe('let')
    // Classes only -- the text is untouched.
    expect(renderedText(e)).toBe(laidOut(e))
  })

  test('numeric literals get the number class', () => {
    expect(
      mountExp(mkLit(42)).querySelector('.scamper-hl-number')?.textContent,
    ).toBe('42')
  })

  test('plain identifiers are not highlighted', () => {
    const el = mountExp(mkApp(mkId('f'), [mkId('x')]))
    expect(el.querySelector('.scamper-hl-keyword')).toBeNull()
  })
})
