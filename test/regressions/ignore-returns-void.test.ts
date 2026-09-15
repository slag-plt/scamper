import { mount } from '@vue/test-utils'
import { expect, test } from 'vitest'
import { runProgram, runProgramValues } from '../harness'
import ValueRenderer from '../../src/lpm/renderers/vue/ValueRenderer.vue'
import HtmlRenderer from '../../src/lpm/renderers/html'
import { isVoid } from '../../src/lpm/util'

// https://github.com/slag-plt/scamper/issues/596
//
// `ignore` is documented `-> void?`, but it hid its argument by handing back a
// `div` styled `display: none`. Hiding the element does not make the value
// void: `(void? (ignore 5))` was `#f`, and `(list (ignore 1) 2)` was a list
// with an HTMLElement in it. Showing nothing is the output pane's job rather
// than the value's, so `ignore` now returns void and *void* is what the web
// renderers draw as a hidden div.

test('ignore returns void, not an element (#596)', async () => {
  const out = await runProgramValues('(ignore 5)')
  expect(out).toHaveLength(1)
  expect(isVoid(out[0])).toBe(true)
  expect(out[0]).not.toBeInstanceOf(HTMLElement)
})

test('a list built from (ignore v) holds void (#596)', async () => {
  expect(
    await runProgram(
      [
        '(define xs (list (ignore 1) 2))',
        '(length xs)',
        '(void? (list-ref xs 0))',
        '(list-ref xs 1)',
      ].join('\n'),
    ),
  ).toEqual(['2', '#t', '2'])
})

test('the output pane draws void as a hidden div (#596)', () => {
  const elt = HtmlRenderer.render(undefined)
  expect(elt.style.display).toBe('none')
  expect(elt.textContent).toBe('')
})

test('the IDE draws void as a hidden div (#596)', () => {
  const pane = mount(ValueRenderer, { props: { value: undefined } })
  expect(pane.text()).toBe('')
  expect(pane.find('div').attributes('style')).toContain('display: none')
})
