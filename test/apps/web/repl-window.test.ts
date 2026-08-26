import { mount } from '@vue/test-utils'
import { EditorView } from '@codemirror/view'
import { getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, describe, expect, test } from 'vitest'
import ReplWindow from '../../../src/app/web/components/ReplWindow.vue'
import type { ReplEntry } from '../../../src/app/web/composables/use-repl'
import { initialize } from '../../../src/scamper'
import '../../../src/app/web/renderers'

await initialize()

function entry(id: number, source: string, values: unknown[] = []): ReplEntry {
  return { id, source, values: values as ReplEntry['values'], isRunning: false }
}

/** The cells on screen, in order; the last one is always the prompt. */
function cells(): string[] {
  return [...document.querySelectorAll('.cm-content')].map((el) =>
    el.textContent.trim(),
  )
}

/** The prompt's editor: the last one in the window. */
function promptView(): EditorView {
  const editors = document.querySelectorAll<HTMLElement>('.cm-editor')
  const view = EditorView.findFromDOM(editors[editors.length - 1])
  if (view === null) throw new Error('the prompt has no editor')
  return view
}

/** Types `text` into the prompt, replacing whatever is there. */
function typeInPrompt(text: string) {
  const view = promptView()
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: text },
  })
}

/** Presses a key in the prompt, which is the last cell. */
function pressInPrompt(key: string, shift = false) {
  const contents = document.querySelectorAll('.cm-content')
  contents[contents.length - 1].dispatchEvent(
    new KeyboardEvent('keydown', { key, shiftKey: shift, bubbles: true }),
  )
}

afterEach(() => {
  document.body.innerHTML = ''
})

// The window is the transcript plus a prompt (#399). What it is seeded from is
// said once, in the banner, rather than beside every entry.
describe('the REPL window', () => {
  test('shows the banner, the entries, and a prompt below them', () => {
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: {
        entries: [entry(0, '(+ 1 2)', [3]), entry(1, '(define x 5)')],
        banner: 'Definitions from lab.scm are available here.',
        isBusy: false,
      },
    })
    try {
      expect(document.body.textContent).toContain('lab.scm')
      expect(cells()).toEqual(['(+ 1 2)', '(define x 5)', ''])
      expect(document.body.textContent).toContain('3')
    } finally {
      wrapper.unmount()
    }
  })

  test('Enter in the prompt submits what was typed', () => {
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: { entries: [], banner: '', isBusy: false },
    })
    try {
      // Dispatched through the view rather than typed: jsdom has no input
      // method, so there is no way to send characters to a contenteditable.
      typeInPrompt('(+ 1 2)')
      pressInPrompt('Enter')
      expect(wrapper.emitted('submit')).toEqual([['(+ 1 2)']])
      // The prompt is cleared for the next entry.
      expect(cells()).toEqual([''])
    } finally {
      wrapper.unmount()
    }
  })

  test('Up recalls the last entry into the prompt', () => {
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: {
        entries: [entry(0, '(+ 1 2)'), entry(1, '(* 3 4)')],
        banner: '',
        isBusy: false,
      },
    })
    try {
      pressInPrompt('ArrowUp')
      expect(cells().at(-1)).toBe('(* 3 4)')
      pressInPrompt('ArrowUp')
      expect(cells().at(-1)).toBe('(+ 1 2)')
      // And back down again, to the empty prompt it started from.
      pressInPrompt('ArrowDown')
      pressInPrompt('ArrowDown')
      expect(cells().at(-1)).toBe('')
    } finally {
      wrapper.unmount()
    }
  })

  test('Stop appears only while an entry is running', async () => {
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: { entries: [], banner: '', isBusy: false },
    })
    try {
      expect(queryByRole(document.body, 'button', { name: /Stop/ })).toBeNull()
      await wrapper.setProps({ isBusy: true })
      getByRole(document.body, 'button', { name: /Stop/ }).click()
      expect(wrapper.emitted('interrupt')).toHaveLength(1)
    } finally {
      wrapper.unmount()
    }
  })

  test('it warns when the file has moved on, and not before', async () => {
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: { entries: [], banner: '', isBusy: false, isStale: false },
    })
    try {
      expect(document.body.textContent).not.toContain('has changed')
      await wrapper.setProps({ isStale: true })
      const warning = getByRole(document.body, 'status')
      expect(warning.textContent).toContain('has changed since this REPL')
      // Above the transcript, not inside it: the transcript scrolls away.
      expect(warning.nextElementSibling?.classList).toContain('repl-scroll')
    } finally {
      wrapper.unmount()
    }
  })

  test('Restart asks for a fresh session', () => {
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: { entries: [], banner: '', isBusy: false },
    })
    try {
      getByRole(document.body, 'button', { name: /Restart/ }).click()
      expect(wrapper.emitted('restart')).toHaveLength(1)
    } finally {
      wrapper.unmount()
    }
  })

  test('a busy prompt refuses to submit again', () => {
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: { entries: [entry(0, '(spin)')], banner: '', isBusy: true },
    })
    try {
      typeInPrompt('(+ 1 2)')
      pressInPrompt('Enter')
      expect(wrapper.emitted('submit')).toBeUndefined()
    } finally {
      wrapper.unmount()
    }
  })
})
