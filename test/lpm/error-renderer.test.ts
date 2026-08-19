import { mount } from '@vue/test-utils'
import { describe, expect, test } from 'vitest'
import VueRenderer from '../../src/lpm/renderers/vue'
import ErrorRenderer from '../../src/lpm/renderers/vue/components/ErrorRenderer.vue'
import { ScamperError } from '../../src/lpm/error'
import { Loc, Range } from '../../src/lpm/range'

// An error is the one thing in the output someone needs to spot without
// reading it, so it is set apart from the plain monospace every other value
// gets -- and its parts are laid out, message first, rather than flattened into
// toString()'s single `Runtime error [2:1-2:8]: ...` line.
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

  test('it leads with the message, not the phase', () => {
    const wrapper = mount(ErrorRenderer, {
      props: { value: new ScamperError('Runtime', 'Variable not found: x') },
    })
    try {
      // The problem is its own element, so it can be read first and on its own.
      expect(wrapper.find('.error-message').text()).toBe('Variable not found: x')
      // The phase is still there, demoted to the second line.
      expect(wrapper.find('.error-origin').text()).toBe('Runtime error')
      // Not the monospace wrapper every other value uses.
      expect(wrapper.find('code').exists()).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })

  test('a plain Error renders without the parts a ScamperError has', () => {
    const wrapper = mount(ErrorRenderer, { props: { value: new Error('boom') } })
    try {
      expect(wrapper.find('.error-message').text()).toBe('boom')
      expect(wrapper.find('.error-origin').exists()).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })

  test('it reports the source location in words', () => {
    const range = new Range(new Loc(2, 1, 12), new Loc(2, 8, 19))
    const wrapper = mount(ErrorRenderer, {
      props: { value: new ScamperError('Runtime', 'boom', undefined, range) },
    })
    try {
      // Where, not `[2:1-2:8]`: the coordinate range is machinery, and the
      // start is the only part of it a reader acts on.
      expect(wrapper.find('.error-origin').text()).toContain('line 2, column 1')
      expect(wrapper.text()).not.toContain('2:1-2:8')
    } finally {
      wrapper.unmount()
    }
  })

  test('a rangeless error says nothing about where', () => {
    const wrapper = mount(ErrorRenderer, {
      props: { value: new ScamperError('Runtime', 'boom') },
    })
    try {
      expect(wrapper.find('.error-origin').text()).not.toContain('line')
    } finally {
      wrapper.unmount()
    }
  })
})
