// @vitest-environment node
import { describe, expect, test } from 'vitest'
import { Linter } from 'eslint'
import { domClassInstanceof } from '../../eslint.config.mjs'

// #595: the `no-restricted-syntax` DOM-class entry added by #535 (PR #579) is
// the durable half of that fix -- it is what stops the next bare `instanceof
// <DOM class>` being written, since such a test throws a `ReferenceError`
// off the browser rather than answering `false`. A selector is code, and this
// one had two holes and a false positive:
//
//   (a) it keyed on `right.name`, so `v instanceof window.HTMLElement` -- the
//       same defect, spelled with a member expression -- escaped entirely;
//   (c) it keyed the guard on `left.left.operator`, so the yoda spelling
//       `'undefined' !== typeof HTMLElement && ...` was flagged although it
//       guards perfectly well, and no `yoda` rule forbids writing it.
//
// A third escape reported with those two -- a guarded expression exempting any
// *descendant*, so `typeof foo !== 'undefined' && [1].some(() => v instanceof
// HTMLElement)` passes -- is deliberately left open, and is not pinned here.
// It is a restatement of the limitation recorded on the rule: a selector
// cannot check that the name guarded is the name tested, so
// `typeof foo !== 'undefined' && v instanceof HTMLElement` escapes with no
// callback involved at all. Narrowing the descendant scope would close none of
// that and would risk flagging honest nesting; only a custom rule could.
//
// The rule object is imported from eslint.config.mjs rather than restated, so
// this pins what the build actually runs.

const linter = new Linter()

/** Whether the real rule reports `code`, which is plain JS on purpose. */
function isFlagged(code: string): boolean {
  const messages = linter.verify(code, {
    languageOptions: { ecmaVersion: 'latest', sourceType: 'module' },
    rules: { 'no-restricted-syntax': ['error', domClassInstanceof] },
  })
  return messages.length > 0
}

describe('#595: the DOM-class `instanceof` lint rule', () => {
  test('the fixtures are otherwise clean, so a report is the rule speaking', () => {
    expect(linter.verify('const a = v instanceof HTMLElement', {})).toEqual([])
  })

  describe('flags an unguarded test, however the class is named', () => {
    test.for([
      ['bare', 'const a = v instanceof HTMLElement'],
      // (a) -- the defect this issue is really about.
      ['qualified by window', 'const a = v instanceof window.HTMLElement'],
      [
        'qualified by globalThis',
        'const a = v instanceof globalThis.AudioContext',
      ],
      ['inside a function', 'function f (v) { return v instanceof Element }'],
    ])('%s', ([, code]) => {
      expect(isFlagged(code)).toBe(true)
    })
  })

  describe('leaves a guarded test alone, in either spelling', () => {
    test.for([
      [
        'the usual spelling',
        "const a = typeof HTMLElement !== 'undefined' && v instanceof HTMLElement",
      ],
      // (c) -- correct code the old selector reported.
      [
        'the yoda spelling',
        "const a = 'undefined' !== typeof HTMLElement && v instanceof HTMLElement",
      ],
      [
        'the negated spelling',
        "const a = typeof HTMLElement === 'undefined' || !(v instanceof HTMLElement)",
      ],
    ])('%s', ([, code]) => {
      expect(isFlagged(code)).toBe(false)
    })
  })

  // The allowlist names DOM classes one by one rather than wildcarding `HTML*`,
  // which is deliberate and load-bearing: `Range` is a class in *this* codebase
  // (src/lpm/range.ts, imported in ~15 files) and `HTMLDisplay` is a local one
  // (src/lpm/output/html.ts), so a wildcard would report honest code. The
  // missing DOM names -- `HTMLCollection`, `NodeList`, `MouseEvent`, `Path2D`
  // and others -- are that restraint's known cost, not an oversight.
  describe('says nothing about a class of our own', () => {
    test.for([
      ['Range', 'const a = v instanceof Range'],
      ['HTMLDisplay', 'const a = v instanceof HTMLDisplay'],
      ['a namespaced error', 'const a = e instanceof LPM.ScamperError'],
    ])('%s', ([, code]) => {
      expect(isFlagged(code)).toBe(false)
    })
  })
})
