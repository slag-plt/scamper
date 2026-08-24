import { mount } from '@vue/test-utils'
import { beforeEach, describe, expect, test } from 'vitest'
import { defineComponent, nextTick, ref, type Ref } from 'vue'
import { providePanels, type Panels } from '../../../src/app/web/composables/use-panels'
import { DEFAULT_LAYOUT, type PanelId } from '../../../src/app/web/panel-layout'

interface Harness {
  panels: Panels
  isCompact: Ref<boolean>
  present: Ref<readonly PanelId[]>
}

/**
 * Mounts a component that provides the store, which is the only place
 * provide() is legal. Each call gets its own instance, which is the isolation
 * the composable is provide-based rather than a module singleton to get.
 */
function withPanels(
  run: (h: Harness) => void | Promise<void>,
  present: readonly PanelId[] = ['editor', 'output'],
) {
  let harness: Harness | null = null
  const Host = defineComponent({
    setup() {
      const isCompact = ref(false)
      const presentRef = ref<readonly PanelId[]>(present)
      harness = {
        panels: providePanels({ isCompact, present: presentRef }),
        isCompact,
        present: presentRef,
      }
      return () => null
    },
  })
  const wrapper = mount(Host)
  return Promise.resolve(run(harness as unknown as Harness)).finally(() => {
    wrapper.unmount()
  })
}

/** A plain-object copy; the layout behind the ref is a reactive proxy. */
function snapshot(value: unknown): unknown {
  return JSON.parse(JSON.stringify(value))
}

const STORAGE_KEY = 'scamper.panels'
const LEGACY_OUTPUT = 'scamper.window.output'
const LEGACY_TRACE = 'scamper.window.trace'

beforeEach(() => {
  localStorage.clear()
})

describe('persistence', () => {
  test('a first run starts from the default and stores nothing yet', async () => {
    await withPanels(({ panels }) => {
      expect(panels.layout.value).toEqual(DEFAULT_LAYOUT)
    })
  })

  test('an arrangement survives a reload', async () => {
    await withPanels(({ panels }) => {
      panels.dock('output')
      panels.setSplitPercent(40)
    })
    await nextTick()

    await withPanels(({ panels }) => {
      expect(panels.tabs('b')).toEqual(['output'])
      expect(panels.layout.value.splitPercent).toBe(40)
    })
  })

  test('a window that was put away comes back on reload', async () => {
    await withPanels(({ panels }) => {
      // The output docks by default (#371); float it first, since only a
      // floating window can be put away.
      panels.float('output')
      panels.minimize('output')
      panels.dock('trace')
    })
    await nextTick()

    await withPanels(({ panels }) => {
      // The arrangement is remembered...
      expect(panels.tabs('b')).toEqual(['trace'])
      // ...but "minimized" is not: opening Scamper to a pane with no output in
      // it, and no obvious way back, is not a state to restore anyone into.
      expect(panels.isVisible('output')).toBe(true)
      expect(panels.minimized.value).toEqual([])
    }, ['editor', 'output', 'trace'])
  })

  test('a blob from a newer build is discarded rather than guessed at', async () => {
    localStorage.setItem(
      STORAGE_KEY,
      JSON.stringify({ version: 99, placement: { editor: { kind: 'floating' } } }),
    )
    await withPanels(({ panels }) => {
      expect(panels.layout.value).toEqual(DEFAULT_LAYOUT)
    })
  })

  test('unparseable storage is discarded rather than fatal', async () => {
    localStorage.setItem(STORAGE_KEY, '{ not json')
    await withPanels(({ panels }) => {
      expect(panels.layout.value).toEqual(DEFAULT_LAYOUT)
    })
  })
})

describe('migrating the old per-window geometry', () => {
  test('it becomes the panel geometry, and the old keys go', async () => {
    localStorage.setItem(LEGACY_OUTPUT, JSON.stringify({ x: 10, y: 20, w: 300, h: 200 }))
    localStorage.setItem(LEGACY_TRACE, JSON.stringify({ x: 1, y: 2, w: 3, h: 4 }))

    await withPanels(({ panels }) => {
      expect(panels.layout.value.geometry.output).toEqual({ x: 10, y: 20, w: 300, h: 200 })
      expect(panels.layout.value.geometry.trace).toEqual({ x: 1, y: 2, w: 3, h: 4 })
    })

    // The store is the only reader of panel geometry now, so the old keys can
    // go -- and must, or every reload would drag the layout back to them.
    expect(localStorage.getItem(LEGACY_OUTPUT)).toBeNull()
    expect(localStorage.getItem(LEGACY_TRACE)).toBeNull()
  })

  test('it does not run a second time and overwrite what someone since arranged', async () => {
    localStorage.setItem(LEGACY_OUTPUT, JSON.stringify({ x: 10, y: 20, w: 300, h: 200 }))
    await withPanels(({ panels }) => {
      panels.setGeometry('output', { x: 99, y: 99, w: 99, h: 99 })
    })
    await nextTick()

    await withPanels(({ panels }) => {
      expect(panels.layout.value.geometry.output).toEqual({ x: 99, y: 99, w: 99, h: 99 })
    })
  })

  test('a malformed legacy value is ignored, not fatal', async () => {
    localStorage.setItem(LEGACY_OUTPUT, '{"x": 1}')
    await withPanels(({ panels }) => {
      expect(panels.layout.value.geometry.output).toBeNull()
    })
  })
})

describe('the narrow-pane projection', () => {
  test('it tabs everything without touching what was arranged', async () => {
    await withPanels(({ panels, isCompact }) => {
      panels.dock('output')
      const arranged = snapshot(panels.layout.value)

      isCompact.value = true
      expect(panels.slots.value).toEqual(['a'])
      expect(panels.tabs('a')).toEqual(['editor', 'output'])
      expect(snapshot(panels.layout.value)).toEqual(arranged)

      isCompact.value = false
      expect(panels.tabs('a')).toEqual(['editor'])
      expect(panels.tabs('b')).toEqual(['output'])
    })
  })

  test('nothing is in the taskbar while compact', async () => {
    await withPanels(({ panels, isCompact }) => {
      panels.float('output')
      panels.minimize('output')
      expect(panels.minimized.value).toEqual(['output'])
      isCompact.value = true
      expect(panels.minimized.value).toEqual([])
    })
  })
})

describe('presence', () => {
  test('a trace that has not been opened is neither a tab nor in the taskbar', async () => {
    await withPanels(({ panels, present }) => {
      panels.minimize('trace')
      expect(panels.minimized.value).toEqual([])

      present.value = ['editor', 'output', 'trace']
      expect(panels.minimized.value).toEqual(['trace'])
    })
  })
})
