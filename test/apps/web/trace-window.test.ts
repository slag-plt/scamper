import { mount } from '@vue/test-utils'
import { getByRole } from '@testing-library/dom'
import { afterEach, describe, expect, test } from 'vitest'
import { nextTick } from 'vue'
import TraceWindow from '../../../src/app/web/components/TraceWindow.vue'
import { initializeLibs } from '../../../src/lib'
import * as SymbolDB from '../../../src/scheme/symbol-db'
import * as Scheme from '../../../src/scheme'
// The AST and trace renderers are registered as an import side effect; without
// them a trace step falls back to the generic struct rendering and never
// reaches the layout renderer that does the highlighting.
import '../../../src/app/web/renderers'
import { mkTraceOutput } from '../../../src/lpm/trace'
import TextRenderer from '../../../src/lpm/renderers/text'
import TraceOutputRenderer from '../../../src/lpm/trace/renderers/TraceOutputRenderer.vue'
import type { Exp } from '../../../src/scheme/ast'
import type { Value } from '../../../src/lpm'

await initializeLibs()
SymbolDB.initialize()

/** The single expression in `src`, as the AST a trace step carries. */
function expressionIn(src: string): Exp {
  const { program } = Scheme.tokenizeAndParse(src)
  const stmt = program?.[0]
  if (stmt?.tag !== 'stmtexp') throw new Error(`${src} is not an expression`)
  return stmt.expr
}

/** A trace of `n` steps, rendered as the numbers 0..n-1. */
function steps(n: number): number[] {
  return Array.from({ length: n }, (_, i) => i)
}

describe('the reduction marker', () => {
  // The web trace is paginated, so a step is already presented on its own and
  // the "-->" says nothing the view does not. The console trace has only
  // consecutive lines, so it keeps the marker. Two renderers, one value.
  const step = mkTraceOutput(42 as unknown as Value)

  test('the web renderer draws no arrow', () => {
    const wrapper = mount(TraceOutputRenderer, { props: { value: step } })
    try {
      expect(wrapper.text()).not.toContain('-->')
      expect(wrapper.text()).toContain('42')
    } finally {
      wrapper.unmount()
    }
  })

  test('the console renderer still does', () => {
    expect(TextRenderer.render(step)).toBe('--> 42')
  })
})

describe('TraceWindow', () => {
  afterEach(() => {
    document.body.innerHTML = ''
  })

  function open(props: Record<string, unknown> = {}) {
    return mount(TraceWindow, {
      props: { source: '(display 1)', steps: steps(35), ...props },
      attachTo: document.body,
    })
  }

  /** The `12/35` reading under the slider. */
  function counter(): string {
    return document.querySelector('.trace-count')?.textContent.trim() ?? ''
  }

  function slider(): HTMLInputElement {
    return getByRole<HTMLInputElement>(document.body, 'slider', { name: 'Step' })
  }

  test('opens on the first step and says where it is', () => {
    const wrapper = open()
    try {
      expect(counter()).toBe('1/35')
      expect(slider().value).toBe('0')
      expect(slider().max).toBe('34')
    } finally {
      wrapper.unmount()
    }
  })

  test('the four buttons move through the trace', async () => {
    const wrapper = open()
    try {
      const click = async (name: string) => {
        getByRole(document.body, 'button', { name }).click()
        await nextTick()
      }

      await click('Next step')
      expect(counter()).toBe('2/35')
      await click('Next step')
      expect(counter()).toBe('3/35')
      await click('Previous step')
      expect(counter()).toBe('2/35')
      await click('Last step')
      expect(counter()).toBe('35/35')
      await click('First step')
      expect(counter()).toBe('1/35')
    } finally {
      wrapper.unmount()
    }
  })

  test('the ends grey out the controls that would run past them', async () => {
    const wrapper = open()
    try {
      const button = (name: string) =>
        getByRole(document.body, 'button', { name })

      // At the first step there is nothing behind it.
      expect(button('First step')).toBeDisabled()
      expect(button('Previous step')).toBeDisabled()
      expect(button('Next step')).toBeEnabled()

      button('Last step').click()
      await nextTick()
      expect(button('Next step')).toBeDisabled()
      expect(button('Last step')).toBeDisabled()
      expect(button('Previous step')).toBeEnabled()
    } finally {
      wrapper.unmount()
    }
  })

  test('the slider seeks anywhere in the trace', async () => {
    const wrapper = open()
    try {
      slider().value = '20'
      slider().dispatchEvent(new Event('input'))
      await nextTick()
      expect(counter()).toBe('21/35')
    } finally {
      wrapper.unmount()
    }
  })

  test('it shows the statement being stepped', () => {
    const wrapper = open({ source: '(+ 1 2)' })
    try {
      const caption = document.querySelector('.source-caption')
      expect(caption?.textContent).toContain('(+ 1 2)')
      // Shown regardless of the output pane's caption option, which is off.
      expect((caption as HTMLElement).style.display).not.toBe('none')
    } finally {
      wrapper.unmount()
    }
  })

  test('a one-step trace disables the slider and both directions', () => {
    const wrapper = open({ steps: steps(1) })
    try {
      expect(counter()).toBe('1/1')
      expect(slider()).toBeDisabled()
      expect(getByRole(document.body, 'button', { name: 'Next step' })).toBeDisabled()
      expect(
        getByRole(document.body, 'button', { name: 'Previous step' }),
      ).toBeDisabled()
    } finally {
      wrapper.unmount()
    }
  })

  test('a statement with no visible steps says so', () => {
    const wrapper = open({ steps: [] })
    try {
      expect(document.body.textContent).toContain('no visible steps')
      expect(counter()).toBe('0/0')
    } finally {
      wrapper.unmount()
    }
  })

  test('a truncated trace says it was cut short', () => {
    const wrapper = open({ steps: steps(50), truncated: true })
    try {
      expect(document.body.textContent).toContain('Stopped after 50 steps')
    } finally {
      wrapper.unmount()
    }
  })

  // Moving between steps highlights the one sub-expression that moved, so the
  // reduction is visible rather than having to be spotted.
  describe('the changed sub-expression', () => {
    /** Trace steps for `(* (+ 1 2) 3)` reducing to 9. */
    function reduction(): Value[] {
      return ['(* (+ 1 2) 3)', '(* 3 3)', '9'].map((src) =>
        mkTraceOutput(expressionIn(src)),
      )
    }

    /** The text inside the highlight, or null when nothing is highlighted. */
    function highlighted(): string | null {
      const el = document.querySelector('.trace-changed')
      return el === null ? null : el.textContent.replace(/\s+/g, '')
    }

    test('the first step highlights nothing, having nothing to differ from', () => {
      const wrapper = open({ steps: reduction() })
      try {
        expect(highlighted()).toBeNull()
      } finally {
        wrapper.unmount()
      }
    })

    test('it marks only the sub-expression that reduced', async () => {
      const wrapper = open({ steps: reduction() })
      try {
        getByRole(document.body, 'button', { name: 'Next step' }).click()
        await nextTick()
        // (* (+ 1 2) 3) --> (* 3 3): the first argument, not the whole call.
        expect(highlighted()).toBe('3')
        expect(document.querySelectorAll('.trace-changed').length).toBe(1)
      } finally {
        wrapper.unmount()
      }
    })

    test('a whole expression collapsing highlights all of it', async () => {
      const wrapper = open({ steps: reduction() })
      try {
        getByRole(document.body, 'button', { name: 'Last step' }).click()
        await nextTick()
        // (* 3 3) --> 9: nothing smaller survived, so the root is the change.
        expect(highlighted()).toBe('9')
      } finally {
        wrapper.unmount()
      }
    })

    test('seeking backwards compares against the new neighbour', async () => {
      const wrapper = open({ steps: reduction() })
      try {
        getByRole(document.body, 'button', { name: 'Last step' }).click()
        await nextTick()
        getByRole(document.body, 'button', { name: 'Previous step' }).click()
        await nextTick()
        // Back on step 2, which differs from step 1 in its first argument.
        expect(highlighted()).toBe('3')
      } finally {
        wrapper.unmount()
      }
    })
  })

  // The close button used to live here. It is PanelFrame's now, along with the
  // rest of the window chrome -- see panel-frame.test.ts.
})
