import { flushPromises, mount } from '@vue/test-utils'
import {
  findByRole,
  fireEvent,
  getByRole,
  queryByRole,
} from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import {
  installMemoryStorage,
  uninstallMemoryStorage,
} from '../../stubs/memory-storage'
import { initialize } from '../../../src/scamper'
import { runProgram } from '../../harness'
import { Fiber } from '../../../src/lpm/fiber'
import { DEFAULT_MAX_CALL_STACK_DEPTH } from '../../../src/lpm/limits'
import { DEFAULT_TRACE_STEP_LIMIT } from '../../../src/lpm/output/trace-collector'
import {
  MAX_CALL_STACK_DEPTH,
  MAX_TRACE_STEP_LIMIT,
  MIN_CALL_STACK_DEPTH,
  liveEvaluation,
  maxCallStackDepth,
  setLiveEvaluation,
  setMaxCallStackDepth,
  setTraceStepLimit,
  traceStepLimit,
} from '../../../src/app/web/run-prefs'
import {
  setShowSourceWithOutput,
  showSourceWithOutput,
} from '../../../src/app/web/output-prefs'
import { currentTheme, setTheme } from '../../../src/theme'
import { useRepl } from '../../../src/app/web/composables/use-repl'
import TextRenderer from '../../../src/lpm/renderers/text'

vi.mock('../../../src/app/web/single-instance', () => ({
  acquireLock: vi.fn(() => Promise.resolve(true)),
  releaseLock: vi.fn(),
  holdsLock: vi.fn(() => true),
}))

vi.mock(
  '../../../src/app/web/components/CodeMirrorEditor.vue',
  () => import('../../stubs/MockCodeMirrorEditor.vue'),
)

vi.mock(
  '../../../src/app/web/components/ResultsPane.vue',
  () => import('../../stubs/MockResultsPane.vue'),
)

await initialize()

// The preferences pane (#497) is one place for settings that used to be a menu
// row each. These tests are about the three things that makes it: it can be
// reached, it agrees with the menus about what is set, and the two settings
// that are numbers -- the only ones with nowhere else to live -- take effect.
describe('preferences pane', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    // These preferences persist themselves, and the modules holding them are
    // process-wide, so each test starts from the defaults rather than from
    // whatever the last one left.
    installMemoryStorage()
    setTheme('light')
    setShowSourceWithOutput(false)
    setLiveEvaluation(true)
    setTraceStepLimit(DEFAULT_TRACE_STEP_LIMIT)
    setMaxCallStackDepth(DEFAULT_MAX_CALL_STACK_DEPTH)
  })

  afterEach(() => {
    setTheme('light')
    setTraceStepLimit(DEFAULT_TRACE_STEP_LIMIT)
    setMaxCallStackDepth(DEFAULT_MAX_CALL_STACK_DEPTH)
    uninstallMemoryStorage()
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  async function mountIde() {
    await fs.saveFile('hello.scm', '(display "hi")')
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    return wrapper
  }

  /** Picks `label` out of the `title` menu, plain item or toggle. */
  async function pick(title: string, label: string) {
    const button = getByRole(document.body, 'menuitem', { name: title })
    if (button.getAttribute('aria-expanded') !== 'true') {
      button.click()
      await flushPromises()
    }
    const menu = getByRole(document.body, 'menu')
    const item =
      queryByRole(menu, 'menuitem', { name: label }) ??
      getByRole(menu, 'menuitemcheckbox', { name: label })
    item.click()
    await flushPromises()
  }

  /** The open pane, or null when it is not showing. */
  function pane() {
    return queryByRole(document.body, 'dialog', { name: 'Preferences' })
  }

  /** Opens the pane from the Edit menu and returns it. */
  async function openPane() {
    await pick('Edit', 'Preferences…')
    return getByRole(document.body, 'dialog', { name: 'Preferences' })
  }

  test('opens from the Edit menu', async () => {
    const wrapper = await mountIde()
    try {
      expect(pane()).toBeNull()
      await openPane()
      expect(pane()).not.toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('opens on Mod+, and closes again', async () => {
    const wrapper = await mountIde()
    try {
      fireEvent.keyDown(window, { key: ',', ctrlKey: true })
      await flushPromises()
      const dialog = getByRole(document.body, 'dialog', { name: 'Preferences' })

      getByRole(dialog, 'button', { name: 'Done' }).click()
      await flushPromises()
      expect(pane()).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  // The chord is captured at the window, so without IdeApp's "not while a
  // dialog is up" guard it would open the pane on top of an open prompt.
  test('Mod+, does nothing while another dialog is up', async () => {
    const wrapper = await mountIde()
    try {
      await pick('File', 'New File…')
      const prompt = await findByRole(document.body, 'dialog', {
        name: 'New file',
      })

      fireEvent.keyDown(window, { key: ',', ctrlKey: true })
      await flushPromises()
      expect(pane()).toBeNull()

      getByRole(prompt, 'button', { name: 'Cancel' }).click()
      await flushPromises()
    } finally {
      wrapper.unmount()
    }
  })

  // The trace step limit was a Run-menu item whose label existed only to show
  // its value (#369); the pane is what let it stop occupying a menu row.
  test('the Run menu no longer carries the trace step limit', async () => {
    const wrapper = await mountIde()
    try {
      getByRole(document.body, 'menuitem', { name: 'Run' }).click()
      await flushPromises()
      expect(
        queryByRole(getByRole(document.body, 'menu'), 'menuitem', {
          name: /trace step limit/i,
        }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('a toggle writes through to its preference and to storage', async () => {
    const wrapper = await mountIde()
    try {
      const box = getByRole(await openPane(), 'checkbox', {
        name: 'Live evaluation',
      })
      expect((box as HTMLInputElement).checked).toBe(true)

      fireEvent.click(box)
      await flushPromises()
      expect(liveEvaluation.value).toBe(false)
      expect(localStorage.getItem('scamper.run.live')).toBe('false')
    } finally {
      wrapper.unmount()
    }
  })

  // The pane and the menus read the same module-level refs, which is what keeps
  // them from disagreeing about what is set.
  test('shows what was set from a menu', async () => {
    const wrapper = await mountIde()
    try {
      await pick('View', 'Source with Output')
      expect(showSourceWithOutput.value).toBe(true)

      const box = getByRole(await openPane(), 'checkbox', {
        name: 'Source with output',
      })
      expect((box as HTMLInputElement).checked).toBe(true)
    } finally {
      wrapper.unmount()
    }
  })

  test('clamps a number past its ceiling, and shows the clamp', async () => {
    const wrapper = await mountIde()
    try {
      const box = getByRole(await openPane(), 'spinbutton', {
        name: 'Trace step limit',
      })

      fireEvent.change(box, { target: { value: '999999999' } })
      await flushPromises()
      expect(traceStepLimit.value).toBe(MAX_TRACE_STEP_LIMIT)
      expect((box as HTMLInputElement).value).toBe(String(MAX_TRACE_STEP_LIMIT))
    } finally {
      wrapper.unmount()
    }
  })

  // The case Vue's own re-render cannot cover: the value is already at the
  // ceiling, so clamping leaves the ref untouched and nothing is scheduled.
  // Only the handler's own write puts the typed number back.
  test('shows the clamp even when the setting does not change', async () => {
    const wrapper = await mountIde()
    try {
      setTraceStepLimit(MAX_TRACE_STEP_LIMIT)
      const box = getByRole(await openPane(), 'spinbutton', {
        name: 'Trace step limit',
      })

      fireEvent.change(box, { target: { value: '999999999' } })
      await flushPromises()
      expect(traceStepLimit.value).toBe(MAX_TRACE_STEP_LIMIT)
      expect((box as HTMLInputElement).value).toBe(String(MAX_TRACE_STEP_LIMIT))
    } finally {
      wrapper.unmount()
    }
  })

  // An empty box reads as 0 through Number, so it is put back rather than
  // clamped up to the floor as a deliberate 5 would be.
  test('puts an emptied number box back', async () => {
    const wrapper = await mountIde()
    try {
      const box = getByRole(await openPane(), 'spinbutton', {
        name: 'Maximum stack depth',
      })

      fireEvent.change(box, { target: { value: '' } })
      await flushPromises()
      expect(maxCallStackDepth.value).toBe(DEFAULT_MAX_CALL_STACK_DEPTH)
      expect((box as HTMLInputElement).value).toBe(
        String(DEFAULT_MAX_CALL_STACK_DEPTH),
      )
    } finally {
      wrapper.unmount()
    }
  })

  test('remembers a depth that was typed in', async () => {
    const wrapper = await mountIde()
    try {
      const box = getByRole(await openPane(), 'spinbutton', {
        name: 'Maximum stack depth',
      })

      fireEvent.change(box, { target: { value: '30000' } })
      await flushPromises()
      expect(maxCallStackDepth.value).toBe(30_000)
      expect(localStorage.getItem('scamper.run.callstackdepth')).toBe('30000')
    } finally {
      wrapper.unmount()
    }
  })

  // The pane reaches every preference module, not just run-prefs; the theme is
  // the one whose effect is visible without asking the editor about it.
  test('reaches a setting that is not a run preference', async () => {
    const wrapper = await mountIde()
    try {
      const box = getByRole(await openPane(), 'checkbox', {
        name: 'Dark theme',
      })
      expect((box as HTMLInputElement).checked).toBe(false)

      fireEvent.click(box)
      await flushPromises()
      expect(currentTheme.value).toBe('dark')
    } finally {
      wrapper.unmount()
    }
  })

  // The pane renders each setting by calling its getter, which is what
  // subscribes it. A value changed while it is open has to show up in it --
  // otherwise the menus and an open pane could drift apart.
  test('follows a setting changed while it is open', async () => {
    const wrapper = await mountIde()
    try {
      const box = getByRole(await openPane(), 'checkbox', {
        name: 'Live evaluation',
      })
      expect((box as HTMLInputElement).checked).toBe(true)

      setLiveEvaluation(false)
      await flushPromises()
      expect((box as HTMLInputElement).checked).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })
})

// The depth preference is the one setting that reaches past the IDE into the
// machine, so what it does there is worth its own tests (#477, #497).
describe('the call-stack-depth preference', () => {
  beforeEach(() => {
    installMemoryStorage()
  })

  afterEach(() => {
    setMaxCallStackDepth(DEFAULT_MAX_CALL_STACK_DEPTH)
    uninstallMemoryStorage()
  })

  test('is the depth a fresh fiber starts at', () => {
    setMaxCallStackDepth(25_000)
    expect(new Fiber([]).maxCallStackDepth).toBe(25_000)
  })

  test('is held to the range the pane offers', () => {
    setMaxCallStackDepth(1)
    expect(maxCallStackDepth.value).toBe(MIN_CALL_STACK_DEPTH)
    setMaxCallStackDepth(10_000_000)
    expect(maxCallStackDepth.value).toBe(MAX_CALL_STACK_DEPTH)
  })

  // The point of the setting: room for the rest of the session, without the
  // program having to ask for it and without the next Run taking it away.
  test('gives a deep recursion room with no call in the program', async () => {
    setMaxCallStackDepth(20_000)
    expect(await runProgram(deepSum(12_000))).toEqual(['72006000'])
  }, 30000)

  // The other half of the bargain: the preference is a default, not the last
  // word, so a program that names its own depth still gets it and stays
  // reproducible wherever it is run.
  test("does not override a program's own set-maximum-recursion-depth!", async () => {
    setMaxCallStackDepth(20_000)
    expect(
      await runProgram(`(set-maximum-recursion-depth! 100)${deepSum(500)}`),
    ).toEqual(['void', 'Runtime error: Max call stack depth 100 exceeded!'])
  })

  // A REPL entry inherits the depth the session is working at, which is what
  // makes the session one continuous program (#477). Inheriting it whether or
  // not anyone chose it would strand a REPL on the depth it opened with -- and
  // a student who has just hit the limit is sitting in exactly that REPL.
  test('reaches a REPL that is already open', async () => {
    setMaxCallStackDepth(MIN_CALL_STACK_DEPTH)
    const repl = useRepl()
    await repl.open('lab.scm', deepSum(0))
    try {
      await repl.submit('(sum 1500)')
      expect(printed(repl)).toContain('Max call stack depth')

      setMaxCallStackDepth(20_000)
      await repl.submit('(sum 1500)')
      expect(printed(repl)).toBe('1125750')
    } finally {
      repl.close()
    }
  })

  // ...unless the session chose one for itself, which still wins.
  test('leaves a REPL that chose its own depth on it', async () => {
    const repl = useRepl()
    await repl.open('lab.scm', deepSum(0))
    try {
      await repl.submit('(set-maximum-recursion-depth! 200)')
      setMaxCallStackDepth(20_000)
      await repl.submit('(sum 1500)')
      expect(printed(repl)).toContain('Max call stack depth 200 exceeded')
    } finally {
      repl.close()
    }
  })
})

/** What the REPL's most recent entry printed, as a person would read it. */
function printed(repl: ReturnType<typeof useRepl>): string {
  const last = repl.entries.value.at(-1)
  if (last === undefined) throw new Error('the REPL has no entries')
  return last.values.map((v) => TextRenderer.render(v)).join('\n')
}

// Deliberately not tail-recursive: one live frame per level is the whole point.
const deepSum = (n: number) => `
(define sum
  (lambda (n)
    (if (= n 0)
        0
        (+ n (sum (- n 1))))))
(sum ${n.toString()})
`
