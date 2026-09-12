import { describe, expect, test } from 'vitest'
import { ICE, ScamperError, Value } from '../../src/lpm'
import * as U from '../../src/lpm/util'
import TextRenderer from '../../src/lpm/renderers/text'

// #545: `TextRenderer.render` (src/lpm/renderers/text.ts) reproduces
// `U.toString`'s dispatch chain (src/lpm/util.ts) branch for branch, by hand --
// and has no `HTMLElement` branch, so the two answer differently for the same
// value: `[HTMLElement]` from one, `[Blob: {}]` from the other. The renderer is
// what a student actually sees (LoggingChannel.send, ConsoleOutput), so the
// wrong answer is the one on screen.
//
// The cases below are the *whole* chain, not just the element: the duplication
// is the defect, so this pins every branch as agreeing rather than only the one
// that has drifted so far.
//
// N.B., this file runs in the suite's default jsdom environment on purpose --
// without a DOM there is no element to disagree about. `HTMLElement` under a
// `typeof` guard outside the browser is #514's territory
// (tostring-without-a-dom.test.ts).
//
// N.B., a custom renderer is *not* covered here, deliberately: registering one
// (src/scheme/ast.ts registers three) is the renderer's extension point, and a
// value it claims -- an AST node -- is meant to render differently from
// `toString`'s view of it.
describe('#545: TextRenderer.render and toString agree', () => {
  function namedFn(x: number): number {
    return x * 3
  }

  const element = () => document.createElement('div')
  const blob = new (class {
    foo = 1
  })()

  const cases: [string, Value][] = [
    ['boolean true', true],
    ['boolean false', false],
    ['number', 42],
    ['string', 'hi'],
    ['string needing escapes', 'a"b\\c\nd'],
    ['void', undefined],
    ['null', null],
    ['empty vector', []],
    ['vector', [1, 2]],
    ['closure', U.mkClosure([], [], [], () => null, 'add-one')],
    ['anonymous closure', U.mkClosure([], [], [], () => null)],
    ['js function', namedFn],
    ['char, unnamed', U.mkChar('a')],
    ['char, named', U.mkChar(' ')],
    ['list', U.mkList(1, 2)],
    ['pair', U.mkPair(1, 2)],
    ['struct with fields', U.mkStruct('point', ['x', 'y'], [1, 2])],
    ['struct with no fields', U.mkStruct('unit', [], [])],
    ['empty map', {}],
    ['map', { a: 1, b: 'hi' }],
    ['ScamperError', new ScamperError('Runtime', 'boom')],
    ['ICE', new ICE('someFn', 'unreachable')],
    ['Error', new Error('oops')],
    ['unrecognized value', blob],
    ['HTMLElement', element()],
    // An element reached through each aggregate, since that is how a program
    // meets one: `(display (list (text "hi")))`.
    ['vector holding an HTMLElement', [element()]],
    ['list holding an HTMLElement', U.mkList(element())],
    ['struct holding an HTMLElement', U.mkStruct('box', ['e'], [element()])],
    ['map holding an HTMLElement', { e: element() }],
  ]

  test.for(cases)('%s', ([, v]) => {
    expect(TextRenderer.render(v)).toBe(U.toString(v))
  })

  // Agreement alone would also be satisfied by dropping the branch from
  // `toString`, which is the wrong repair: an element has a name, and
  // `[Blob: {}]` tells a student nothing. So pin the answer, not just the
  // agreement.
  test('and the answer they agree on names the element', () => {
    expect(U.toString(element())).toBe('[HTMLElement]')
    expect(TextRenderer.render(element())).toBe('[HTMLElement]')
  })
})
