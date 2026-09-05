import { mount } from '@vue/test-utils'
import { EditorView } from '@codemirror/view'
import { getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, describe, expect, test } from 'vitest'
import { nextTick } from 'vue'
import ReplWindow from '../../../src/app/web/components/ReplWindow.vue'
import type { ReplEntry } from '../../../src/app/web/composables/use-repl'
import { Env } from '../../../src/lpm'
import { initialize } from '../../../src/scamper'
import '../../../src/app/web/renderers'

await initialize()

/**
 * A finished entry, as `submit` leaves one that compiled and ran: `isRunning`
 * back to false, `ran` true, and holding the top level it ran in. The window is
 * what these specs are about, so every fixture is an entry it would really be
 * handed.
 */
function entry(
  id: number,
  source: string,
  values: ReplEntry['values'] = [],
): ReplEntry {
  return { id, source, values, isRunning: false, ran: true, env: Env.empty }
}

/**
 * An entry that was refused or did not compile: it is in the transcript, so it
 * can be read and fixed, but it never ran and so cannot be stepped.
 */
function refusedEntry(id: number, source: string): ReplEntry {
  return { id, source, values: [], isRunning: false, ran: false, env: null }
}

/** The step button on the `n`th entry, or null when it offers none. */
function stepButton(n: number): HTMLButtonElement | null {
  return document
    .querySelectorAll('.repl-entry')
    [n].querySelector<HTMLButtonElement>('.repl-step')
}

/**
 * Right-clicks what was typed in the `n`th entry -- where the menu is offered,
 * as opposed to over its output.
 *
 * @returns whether the browser's own menu survived it.
 */
function rightClick(n: number): boolean {
  const event = new MouseEvent('contextmenu', { bubbles: true, cancelable: true })
  const source = document.querySelectorAll('.repl-entry')[n].querySelector('.repl-source')
  if (source === null) throw new Error(`entry ${String(n)} shows no source`)
  return source.dispatchEvent(event)
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
        // What Up walks is the history, not the transcript beside it.
        history: ['(+ 1 2)', '(* 3 4)'],
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

  test('only the prompt is a document the language server holds', () => {
    // One document per live cell: the entries above are a record, and one each
    // would be a document per line ever typed.
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: {
        entries: [entry(0, '(+ 1 2)', [3])],
        banner: '',
        isBusy: false,
        context: '(define x 1)',
      },
    })
    try {
      // The prompt is the last editor, and the only one with completion wired
      // to it; a read-only entry has no such plugin.
      const editors = document.querySelectorAll('.cm-editor')
      expect(editors.length).toBe(2)
      expect(promptView().state.doc.toString()).toBe('')
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

// An entry that ran can be stepped, from a button on it or from its right-click
// menu (#424). Both hand back the entry itself, since it carries the top level
// the trace has to be replayed in.
describe('stepping an entry', () => {
  test('a button on an entry that ran asks to step it', () => {
    const stepped = entry(0, '(+ 1 2)', [3])
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: { entries: [stepped], banner: '', isBusy: false },
    })
    try {
      const button = stepButton(0)
      expect(button).not.toBeNull()
      button?.click()
      // The entry itself is handed back: it carries the top level the trace has
      // to be replayed in.
      expect(wrapper.emitted('step')).toEqual([[stepped]])
    } finally {
      wrapper.unmount()
    }
  })

  test('an entry that never ran offers no button', () => {
    // It was refused or did not compile, so there is nothing to replay.
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: {
        entries: [refusedEntry(0, '(display')],
        banner: '',
        isBusy: false,
      },
    })
    try {
      expect(stepButton(0)).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('output from a handler offers no button', () => {
    // Nobody typed it, so there is no statement to step.
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: {
        entries: [{ ...refusedEntry(0, ''), values: [7] }],
        banner: '',
        isBusy: false,
      },
    })
    try {
      expect(stepButton(0)).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('the button stays in the document rather than appearing on hover', () => {
    // It is faded by CSS, not added and removed: a control that only exists
    // while the pointer is over it cannot be tabbed to. jsdom applies no
    // scoped styles, so what is pinned here is the part that matters -- that
    // it is rendered, labelled, and reachable at rest.
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: { entries: [entry(0, '(+ 1 2)')], banner: '', isBusy: false },
    })
    try {
      const button = stepButton(0)
      expect(button?.isConnected).toBe(true)
      expect(button?.getAttribute('aria-label')).toBe('Step through this')
      expect(button?.disabled).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })

  test('the offer is withdrawn while another trace is collecting', async () => {
    // One at a time, as the Step in the menu bar is: a control that can be
    // clicked and do nothing is worse than one that says it cannot.
    const stepped = entry(0, '(+ 1 2)')
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: { entries: [stepped], banner: '', isBusy: false },
    })
    try {
      await wrapper.setProps({ isStepping: true })
      expect(stepButton(0)?.disabled).toBe(true)
      stepButton(0)?.click()
      expect(wrapper.emitted('step')).toBeUndefined()

      // ...and the menu says so too, rather than quietly dropping the item.
      expect(rightClick(0)).toBe(false)
      await nextTick()
      expect(
        getByRole(document.body, 'menuitem', { name: /Step through this/ })
          .getAttribute('aria-disabled'),
      ).toBe('true')
    } finally {
      wrapper.unmount()
    }
  })

  test('right-clicking what an entry printed keeps the native menu', () => {
    // Output holds pictures and players with right-click menus of their own,
    // and those are the browser's to give.
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: {
        entries: [entry(0, '(+ 1 2)', [3])],
        banner: '',
        isBusy: false,
      },
    })
    try {
      const value = document.querySelector('.repl-value')
      const event = new MouseEvent('contextmenu', {
        bubbles: true,
        cancelable: true,
      })
      expect(value?.dispatchEvent(event)).toBe(true)
      expect(
        queryByRole(document.body, 'menuitem', { name: /Step through this/ }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('right-clicking an entry that ran offers to step it', async () => {
    const stepped = entry(0, '(+ 1 2)')
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: { entries: [stepped], banner: '', isBusy: false },
    })
    try {
      // Cancelled, so the browser's own menu makes way for ours.
      expect(rightClick(0)).toBe(false)
      await nextTick()
      const item = getByRole(document.body, 'menuitem', {
        name: /Step through this/,
      })
      item.click()
      expect(wrapper.emitted('step')).toEqual([[stepped]])
    } finally {
      wrapper.unmount()
    }
  })

  test('right-clicking an entry that never ran keeps the native menu', async () => {
    // Nothing of ours to offer, so taking Copy away would give nothing back.
    const wrapper = mount(ReplWindow, {
      attachTo: document.body,
      props: {
        entries: [refusedEntry(0, '(display')],
        banner: '',
        isBusy: false,
      },
    })
    try {
      expect(rightClick(0)).toBe(true)
      await nextTick()
      expect(
        queryByRole(document.body, 'menuitem', { name: /Step through this/ }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })
})
