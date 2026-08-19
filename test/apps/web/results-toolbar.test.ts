import { mount } from '@vue/test-utils'
import { nextTick } from 'vue'
import { getByRole } from '@testing-library/dom'
import { afterEach, describe, expect, test } from 'vitest'
import ResultsToolbar from '../../../src/app/web/components/ResultsToolbar.vue'
import {
  setShowSourceWithOutput,
  showSourceWithOutput,
} from '../../../src/app/web/output-prefs'

const NOOP_PROPS = { isDirty: false }

describe('ResultsToolbar', () => {
  afterEach(() => {
    setShowSourceWithOutput(false)
  })

  test('it offers the source switch and nothing else', () => {
    // Stepping moved to its own window; the buttons that drove the old
    // in-place trace were left pointing at a mode nothing starts any more.
    const wrapper = mount(ResultsToolbar, {
      props: NOOP_PROPS,
      attachTo: document.body,
    })
    try {
      const buttons = [...document.querySelectorAll('button')].map((b) =>
        b.getAttribute('aria-label'),
      )
      expect(buttons).toEqual(['Show source with output'])
    } finally {
      wrapper.unmount()
    }
  })

  test('its source switch is the same preference the View menu sets', async () => {
    const wrapper = mount(ResultsToolbar, {
      props: NOOP_PROPS,
      attachTo: document.body,
    })
    try {
      const toggle = getByRole(document.body, 'switch', {
        name: 'Show source with output',
      })
      expect(toggle).toHaveAttribute('aria-checked', 'false')

      toggle.click()
      await nextTick()
      expect(showSourceWithOutput.value).toBe(true)
      expect(toggle).toHaveAttribute('aria-checked', 'true')

      // Set from elsewhere (the menu does exactly this), the button follows:
      // neither side keeps its own copy of the flag.
      setShowSourceWithOutput(false)
      await nextTick()
      expect(toggle).toHaveAttribute('aria-checked', 'false')
    } finally {
      wrapper.unmount()
    }
  })
})
