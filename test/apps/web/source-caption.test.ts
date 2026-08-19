import { mount } from '@vue/test-utils'
import { nextTick } from 'vue'
import { afterEach, describe, expect, test } from 'vitest'
import SourceCaption from '../../../src/app/web/components/SourceCaption.vue'
import {
  setShowSourceWithOutput,
  showSourceWithOutput,
} from '../../../src/app/web/output-prefs'
import { highlightScamper } from '../../../src/app/web/source-highlight'
import { initializeLibs } from '../../../src/lib'

await initializeLibs()

describe('highlightScamper', () => {
  test('covers the source exactly, gaps included', () => {
    const src = '(display "hi") ; a note'
    expect(highlightScamper(src).map((t) => t.text).join('')).toBe(src)
  })

  test('tags the pieces a reader distinguishes by colour', () => {
    const classOf = (src: string, text: string) =>
      highlightScamper(src).find((t) => t.text === text)?.cls

    expect(classOf('(define x 5)', 'define')).toBe('scamper-hl-keyword')
    expect(classOf('(display 42)', '42')).toBe('scamper-hl-number')
    expect(classOf('(display "hi")', '"hi"')).toBe('scamper-hl-string')
    expect(classOf('; note\n(display 1)', '; note')).toBe('scamper-hl-comment')
    // Plain identifiers are left alone, as they are in the editor.
    expect(classOf('(display x)', 'x')).toBeUndefined()
  })

  test('does not choke on source that does not parse', () => {
    // A caption is built from a range in a program that compiled, but the
    // slice can still be odd; it must never throw into the output pane.
    expect(() => highlightScamper('(display')).not.toThrow()
    expect(highlightScamper('(display').map((t) => t.text).join('')).toBe(
      '(display',
    )
  })
})

describe('SourceCaption', () => {
  afterEach(() => {
    setShowSourceWithOutput(false)
  })

  test('is rendered but hidden until the option is on', async () => {
    const wrapper = mount(SourceCaption, {
      props: { source: '(display 1)' },
      attachTo: document.body,
    })
    try {
      const box = wrapper.find('.source-caption')
      // In the DOM either way -- that is what makes the toggle retroactive.
      expect(box.exists()).toBe(true)
      expect(box.attributes('style')).toContain('display: none')
      expect(box.text()).toContain('(display 1)')

      setShowSourceWithOutput(true)
      await nextTick()
      expect(wrapper.find('.source-caption').attributes('style') ?? '').not.toContain(
        'display: none',
      )
    } finally {
      wrapper.unmount()
    }
  })

  test('renders the source as highlighted spans, not as editable text', () => {
    const wrapper = mount(SourceCaption, {
      props: { source: '(define x 5)' },
      attachTo: document.body,
    })
    try {
      expect(wrapper.find('.scamper-hl-keyword').text()).toBe('define')
      expect(wrapper.find('.scamper-hl-number').text()).toBe('5')
      // Nothing a person can type into.
      expect(wrapper.find('textarea').exists()).toBe(false)
      expect(wrapper.find('input').exists()).toBe(false)
      expect(wrapper.find('[contenteditable]').exists()).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })

  test('the preference survives a remount', () => {
    setShowSourceWithOutput(true)
    expect(showSourceWithOutput.value).toBe(true)
    expect(localStorage.getItem('scamper.output.showSource')).toBe('true')
  })
})
