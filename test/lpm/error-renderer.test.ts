import { mount } from '@vue/test-utils'
import { describe, expect, test } from 'vitest'
import VueRenderer from '../../src/lpm/renderers/vue'
import ErrorRenderer from '../../src/lpm/renderers/vue/components/ErrorRenderer.vue'
import { ScamperError } from '../../src/lpm/error'
import { Loc, Range } from '../../src/lpm/range'

// An error is the one thing in the output someone needs to spot without
// reading it, so it is rendered as bold italic text rather than as the plain
// monospace every other value gets.
describe('error rendering', () => {
  test('an error picks the error renderer, not the text fallback', () => {
    expect(VueRenderer.render(new ScamperError('Runtime', 'boom'))).toBe(
      ErrorRenderer,
    )
    // Plain Errors too -- a bug escaping the runtime should stand out just as
    // much as a Scamper-level one.
    expect(VueRenderer.render(new Error('boom'))).toBe(ErrorRenderer)
  })

  test('a non-error still renders the ordinary way', () => {
    expect(VueRenderer.render(42)).not.toBe(ErrorRenderer)
    expect(VueRenderer.render('hello')).not.toBe(ErrorRenderer)
  })

  test('it renders the error text, emphasized', () => {
    const wrapper = mount(ErrorRenderer, {
      props: { value: new ScamperError('Runtime', 'Variable not found: x') },
    })
    try {
      expect(wrapper.text()).toBe('Runtime error: Variable not found: x')
      // Bold via the element itself, italic via the class it carries.
      expect(wrapper.find('strong.error-text').exists()).toBe(true)
      // Not the monospace wrapper every other value uses.
      expect(wrapper.find('code').exists()).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })

  test('it keeps the source range the message carries', () => {
    const range = new Range(new Loc(2, 1, 12), new Loc(2, 8, 19))
    const wrapper = mount(ErrorRenderer, {
      props: { value: new ScamperError('Runtime', 'boom', undefined, range) },
    })
    try {
      expect(wrapper.text()).toContain('2:1-2:8')
    } finally {
      wrapper.unmount()
    }
  })
})
