import { mount } from '@vue/test-utils'
import { expect, test } from 'vitest'
import ValueRenderer from '../../src/lpm/renderers/vue/ValueRenderer.vue'
import ModalContents from '../../src/app/web/components/query/modal/ModalContents.vue'
import { mkCons } from '../../src/lpm/util'
import type { Value } from '../../src/lpm/lang'

// https://github.com/slag-plt/scamper/issues/444
//
// ValueRenderer declared its prop from the type `Value`, a union that includes
// `boolean`, so Vue's compiler put `Boolean` in the prop's runtime type. That
// switches on Vue's HTML boolean-attribute casting, which turns an incoming
// `''` -- or the prop's own name, `"value"` -- into `true`, so the empty string
// reached the renderer already a boolean and printed as `#t`. The renderer
// itself was never wrong: it picks the string strategy for `''` and the text
// and CLI renderers print it correctly, which is why only the IDE showed this.

test('the empty string renders as "" rather than #t (#444)', () => {
  expect(mount(ValueRenderer, { props: { value: '' } }).text()).toBe('""')
  expect(mount(ModalContents, { props: { value: '' } }).text()).toBe('""')
})

// The other half of Vue's casting rule: a value equal to the prop's own
// hyphenated name was cast to `true` too, so this string was never empty and
// still printed as #t.
test('a string equal to the prop\'s name renders as itself (#444)', () => {
  expect(mount(ValueRenderer, { props: { value: 'value' } }).text()).toBe(
    '"value"',
  )
  expect(mount(ModalContents, { props: { value: 'value' } }).text()).toBe(
    '"value"',
  )
})

test('an empty string inside a list renders as "" (#444)', () => {
  const list = mkCons('a', mkCons('', mkCons('b', null)))
  expect(mount(ValueRenderer, { props: { value: list } }).text()).toBe(
    '(list"a""""b")',
  )
})

test('other values are unaffected (#444)', () => {
  const cases: [Value, string][] = [
    ['hi', '"hi"'],
    [true, '#t'],
    [false, '#f'],
    [0, '0'],
    [null, 'null'],
    [undefined, 'void'],
  ]
  for (const [value, expected] of cases) {
    expect(mount(ValueRenderer, { props: { value } }).text()).toBe(expected)
  }
})

// The fix rewrote ModalContents' `clip` from withDefaults to a runtime default,
// so this pins the behaviour that rewrite had to preserve.
test('ModalContents still clips by default (#444)', () => {
  expect(
    mount(ModalContents, { props: { value: '' } }).find('#query-contents')
      .classes(),
  ).toContain('clip')
  expect(
    mount(ModalContents, { props: { value: '', clip: false } })
      .find('#query-contents')
      .classes(),
  ).not.toContain('clip')
})
