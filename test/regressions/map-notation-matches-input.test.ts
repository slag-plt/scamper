import { mount } from '@vue/test-utils'
import { describe, expect, test } from 'vitest'
import type { Value } from '../../src/lpm'
import HtmlRenderer from '../../src/lpm/renderers/html'
import TextRenderer from '../../src/lpm/renderers/text'
import ValueRenderer from '../../src/lpm/renderers/vue/ValueRenderer.vue'
import * as U from '../../src/lpm/util'
import { initialize } from '../../src/scamper'
import { runProgram } from '../harness.js'
import '../../src/app/web/renderers'

await initialize()

// https://github.com/slag-plt/scamper/issues/751
//
// A map was read one way and printed another: a student typed
// `{"a" 1 "b" 2}` and Scamper answered `{ "a" : 1, "b" : 2 }`. That second
// form is not Scamper syntax -- there are no colons and no commas in the
// language -- so the one value a student could see but not type back was the
// one whose literal they had just written.
//
// Both halves of the issue are pinned here. The printed form is now the
// literal (part 2), and `make-hash` builds a map without braces (part 1), so
// the type can be used entirely through ordinary calls.
// `.element.textContent`, not test-utils' `.text()`: the separators are
// whitespace-only `CodeElement`s (the convention ListRenderer.vue uses too),
// and `.text()` drops those, reporting `{"a"1"b"2}` for a correct rendering.
const vueText = (value: Value): string | null =>
  (mount(ValueRenderer, { props: { value } }).element as HTMLElement)
    .textContent

describe('#751: a map prints the way it is written', () => {
  // Three renderers draw a value and all three are asserted together, as in
  // empty-list-spelling.test.ts: #668 was one renderer of three drifting, and
  // the rule only reads as a rule when the surfaces sit side by side.
  const map = { a: 1, b: 2 }

  test('toString and the text renderer', () => {
    expect(U.toString(map)).toBe('{"a" 1 "b" 2}')
    expect(TextRenderer.render(map)).toBe('{"a" 1 "b" 2}')
  })

  test('the html renderer', () => {
    expect(HtmlRenderer.render(map).textContent).toBe('{"a" 1 "b" 2}')
  })

  test('the vue renderer', () => {
    expect(vueText(map)).toBe('{"a" 1 "b" 2}')
  })

  test('the empty map is {} on every surface', () => {
    expect(U.toString({})).toBe('{}')
    expect(TextRenderer.render({})).toBe('{}')
    expect(HtmlRenderer.render({}).textContent).toBe('{}')
    expect(vueText({})).toBe('{}')
  })
})

// The property the issue is really asking for, and the one a format change
// could pass the assertions above while still breaking: what Scamper prints
// is a program Scamper reads back as the same value. Asserted by feeding each
// rendering in as source, which is also why the keys stay quoted -- a map key
// is a string, so `{a 1}` would read `a` as a variable.
describe('#751: a printed map is valid input', () => {
  test.each([
    ['{}', '{}'],
    ['{"a" 1}', '{"a" 1}'],
    ['{"a" 1 "b" 2}', '{"a" 1 "b" 2}'],
    ['{"outer" {"inner" 3}}', '{"outer" {"inner" 3}}'],
    ['{"xs" (list 1 2)}', '{"xs" (list 1 2)}'],
    ['{"v" (vector 1 2)}', '{"v" (vector 1 2)}'],
    // A key that needs escaping survives the trip, which is the case a
    // hand-rolled join of the keys would get wrong.
    ['{"a \\"b\\"" 1}', '{"a \\"b\\"" 1}'],
    ['{"" 1}', '{"" 1}'],
    // `__proto__` is an ordinary key, not the prototype setter a plain
    // assignment would have run: before #751 this printed as `{}`.
    ['{"__proto__" 1}', '{"__proto__" 1}'],
    ['{"__proto__" {"a" 1}}', '{"__proto__" {"a" 1}}'],
  ])('%s round-trips', async (src, printed) => {
    expect(await runProgram(src)).toEqual([printed])
    // The rendering, fed back in, is the same value again -- not merely
    // something that parses.
    expect(await runProgram(printed)).toEqual([printed])
  })
})

// Javascript orders an object's own keys with the array-index-like ones
// first, ascending, so a map written with numeric keys prints in a different
// order than it was typed. The *value* still reads back as the same map,
// which is the property above; the order does not survive, and no claim here
// or in the patch notes says it does.
describe('#751: a numeric key is reordered, and still round-trips', () => {
  test('the printed order is normalised, not the written one', async () => {
    expect(await runProgram('{"b" 1 "1" 2}')).toEqual(['{"1" 2 "b" 1}'])
  })

  test('the value is unchanged by the reordering', async () => {
    expect(await runProgram('(equal? {"b" 1 "1" 2} {"1" 2 "b" 1})')).toEqual([
      '#t',
    ])
    expect(await runProgram('{"1" 2 "b" 1}')).toEqual(['{"1" 2 "b" 1}'])
  })
})

describe('#751: make-hash builds a map without braces', () => {
  test('with no arguments it is the empty map', async () => {
    expect(await runProgram('(make-hash)')).toEqual(['{}'])
  })

  test('its arguments alternate keys and values, as the literal does', async () => {
    expect(await runProgram('(make-hash "a" 1 "b" 2)')).toEqual([
      '{"a" 1 "b" 2}',
    ])
    expect(await runProgram('(equal? (make-hash "a" 1) {"a" 1})')).toEqual([
      '#t',
    ])
  })

  test('a __proto__ key is an ordinary key here too', async () => {
    expect(await runProgram('(make-hash "__proto__" 5)')).toEqual([
      '{"__proto__" 5}',
    ])
    expect(await runProgram('(hash-ref (make-hash "__proto__" 5) "__proto__")')).toEqual([
      '5',
    ])
    expect(await runProgram('(hash? (make-hash "__proto__" {}))')).toEqual([
      '#t',
    ])
  })

  test('it composes with the rest of the hash interface', async () => {
    expect(await runProgram('(hash-set (make-hash) "x" 9)')).toEqual([
      '{"x" 9}',
    ])
    expect(await runProgram('(hash-count (make-hash "a" 1 "b" 2))')).toEqual([
      '2',
    ])
  })

  // The literal's own two rules, which `make-hash` shares because it shares
  // the implementation. The message is not self-named: the call-site prefix
  // supplies `(make-hash)`, and adding a second one is #685's defect.
  test('an odd argument count is an error', async () => {
    expect(await runProgram('(make-hash "a")', { stripRanges: true })).toEqual([
      'Runtime error: (make-hash) A map requires an even number of arguments (alternating keys and values), received 1',
    ])
  })

  test('a non-string key is an error', async () => {
    expect(await runProgram('(make-hash 1 2)', { stripRanges: true })).toEqual([
      'Runtime error: (make-hash) A map key must be a string, received number',
    ])
  })
})
