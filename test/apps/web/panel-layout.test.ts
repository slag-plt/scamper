import { describe, expect, test } from 'vitest'
import {
  activeIn,
  compact,
  DEFAULT_LAYOUT,
  dock,
  float,
  floatingZ,
  isVisible,
  minimize,
  moveToOtherSlot,
  normalize,
  occupiedSlots,
  PANEL_IDS,
  reveal,
  setGeometry,
  setSplitPercent,
  tabsIn,
  type PanelId,
  type PanelLayout,
} from '../../../src/app/web/panel-layout'

// The panel model is pure data on purpose: jsdom lays nothing out, so every
// decision has to be answerable without pixels. This is where the coverage
// lives; only the geometry that needs a real box belongs in a browser test.

const ALL = PANEL_IDS
/** Before anything is stepped, the trace does not exist. */
const NO_TRACE: PanelId[] = ['editor', 'output']
/**
 * The editor docked alone, with the output floating over it -- the default
 * arrangement until #371 docked the output. The cases below about a one-panel
 * dock, or about putting the output away, need it floating.
 *
 * Recency is restored to the default's, because `float` fronts what it floats
 * and the startup tie-break (trace, output, editor) is itself under test.
 */
const OUTPUT_FLOATING: PanelLayout = {
  ...float(DEFAULT_LAYOUT, 'output'),
  recency: [...DEFAULT_LAYOUT.recency],
}

/** Every panel is in exactly one place, and recency lists each exactly once. */
function checkInvariants(l: PanelLayout) {
  expect(Object.keys(l.placement).sort()).toEqual([...ALL].sort())
  expect([...l.recency].sort()).toEqual([...ALL].sort())
  expect(l.splitPercent).toBeGreaterThanOrEqual(15)
  expect(l.splitPercent).toBeLessThanOrEqual(85)
  // Slot B is never occupied while slot A is empty.
  const inA = ALL.filter((id) => l.placement[id].kind === 'docked' && l.placement[id].slot === 'a')
  const inB = ALL.filter((id) => l.placement[id].kind === 'docked' && l.placement[id].slot === 'b')
  if (inB.length > 0) expect(inA.length).toBeGreaterThan(0)
}

describe('normalize', () => {
  test('a garbage value becomes the default layout', () => {
    for (const junk of [undefined, null, 42, 'nope', [], { placement: 7 }]) {
      const l = normalize(junk)
      checkInvariants(l)
      expect(l).toEqual(DEFAULT_LAYOUT)
    }
  })

  test('an unknown panel is dropped and a missing one restored', () => {
    const l = normalize({
      placement: { editor: { kind: 'docked', slot: 'a' }, ghost: { kind: 'docked', slot: 'b' } },
      recency: ['ghost', 'output', 'output'],
    })
    checkInvariants(l)
    // The duplicate is collapsed, and the absent ones are appended in the
    // default's priority order rather than the canonical one.
    expect(l.recency).toEqual(['output', 'trace', 'editor'])
  })

  test('a nonsense placement falls back to that panel default', () => {
    const l = normalize({ placement: { output: { kind: 'sideways' } } })
    expect(l.placement.output).toEqual(DEFAULT_LAYOUT.placement.output)
  })

  test('a geometry survives only if it is really a box', () => {
    const good = normalize({ geometry: { output: { x: 1, y: 2, w: 3, h: 4 } } })
    expect(good.geometry.output).toEqual({ x: 1, y: 2, w: 3, h: 4 })

    const bad = normalize({ geometry: { output: { x: 1, y: 2 } } })
    expect(bad.geometry.output).toBeNull()
  })

  test('geometry is total over the panels, like placement is', () => {
    const l = normalize({ geometry: { output: { x: 1, y: 2, w: 3, h: 4 } } })
    expect(Object.keys(l.geometry).sort()).toEqual([...ALL].sort())
  })

  test('splitPercent is clamped, and NaN falls back', () => {
    expect(normalize({ splitPercent: 5 }).splitPercent).toBe(15)
    expect(normalize({ splitPercent: 200 }).splitPercent).toBe(85)
    expect(normalize({ splitPercent: Number.NaN }).splitPercent).toBe(62)
    expect(normalize({ splitPercent: 'wide' }).splitPercent).toBe(62)
  })

  test('an occupied B with an empty A collapses into A', () => {
    const l = normalize({
      placement: {
        editor: { kind: 'floating', geometry: null, minimized: false },
        output: { kind: 'docked', slot: 'b' },
        trace: { kind: 'docked', slot: 'b' },
      },
    })
    checkInvariants(l)
    expect(tabsIn(l, 'a', ALL)).toEqual(['output', 'trace'])
    expect(tabsIn(l, 'b', ALL)).toEqual([])
  })
})

describe('the verbs keep every panel in exactly one place', () => {
  const verbs: [string, (l: PanelLayout) => PanelLayout][] = [
    ['reveal', (l) => reveal(l, 'trace')],
    ['minimize', (l) => minimize(l, 'output')],
    ['float', (l) => float(l, 'editor')],
    ['dock', (l) => dock(l, 'output')],
    ['moveToOtherSlot', (l) => moveToOtherSlot(l, 'editor')],
    ['setSplitPercent', (l) => setSplitPercent(l, 40)],
    ['setGeometry', (l) => setGeometry(l, 'output', { x: 0, y: 0, w: 10, h: 10 })],
  ]

  for (const [name, verb] of verbs) {
    test(name, () => {
      // From both arrangements: the default is two docked slots, so a verb
      // that acts on a floating output (minimize) or docks one (dock) would
      // otherwise be a no-op and prove nothing.
      checkInvariants(verb(DEFAULT_LAYOUT))
      checkInvariants(verb(OUTPUT_FLOATING))
    })
  }
})

describe('docking', () => {
  test('dock always targets the far side', () => {
    const l = dock(OUTPUT_FLOATING, 'output')
    expect(tabsIn(l, 'a', ALL)).toEqual(['editor'])
    expect(tabsIn(l, 'b', ALL)).toEqual(['output'])
  })

  test('moveToOtherSlot turns a split into tabs', () => {
    const split = dock(OUTPUT_FLOATING, 'output')
    const tabbed = moveToOtherSlot(split, 'output')
    expect(tabsIn(tabbed, 'a', ALL)).toEqual(['editor', 'output'])
    expect(occupiedSlots(tabbed, ALL)).toEqual(['a'])
  })

  test('moveToOtherSlot turns tabs back into a split', () => {
    const tabbed = moveToOtherSlot(dock(OUTPUT_FLOATING, 'output'), 'output')
    const split = moveToOtherSlot(tabbed, 'output')
    expect(tabsIn(split, 'a', ALL)).toEqual(['editor'])
    expect(tabsIn(split, 'b', ALL)).toEqual(['output'])
  })

  test('moving the only docked panel is a no-op, not an empty dock', () => {
    // It would land in B, and normalize pulls it straight back to A: there is
    // nothing for it to sit beside.
    const moved = moveToOtherSlot(OUTPUT_FLOATING, 'editor')
    expect(tabsIn(moved, 'a', ALL)).toEqual(['editor'])
    expect(occupiedSlots(moved, ALL)).toEqual(['a'])
  })

  test('floating the last docked panel leaves the dock empty, which is allowed', () => {
    const l = float(OUTPUT_FLOATING, 'editor')
    checkInvariants(l)
    expect(occupiedSlots(l, ALL)).toEqual([])
  })

  test('docking and floating again returns the window to where it was', () => {
    // Geometry lives beside placement rather than inside it, so the round trip
    // does not throw the box away and drop the window back in the corner.
    const placed = setGeometry(OUTPUT_FLOATING, 'output', { x: 7, y: 9, w: 300, h: 200 })
    const round = float(dock(placed, 'output'), 'output')
    expect(round.geometry.output).toEqual({ x: 7, y: 9, w: 300, h: 200 })
  })

  test('a docked panel still remembers where it floated', () => {
    const placed = setGeometry(OUTPUT_FLOATING, 'output', { x: 7, y: 9, w: 300, h: 200 })
    expect(dock(placed, 'output').geometry.output).toEqual({ x: 7, y: 9, w: 300, h: 200 })
  })

  test('dock and float are each a no-op when already there', () => {
    expect(dock(dock(OUTPUT_FLOATING, 'output'), 'output')).toEqual(
      dock(OUTPUT_FLOATING, 'output'),
    )
    expect(float(OUTPUT_FLOATING, 'output')).toEqual(OUTPUT_FLOATING)
  })
})

describe('recency', () => {
  test('reveal fronts a panel and un-minimizes it', () => {
    const away = minimize(OUTPUT_FLOATING, 'output')
    expect(isVisible(away, 'output')).toBe(false)
    const back = reveal(away, 'output')
    expect(isVisible(back, 'output')).toBe(true)
    expect(back.recency[0]).toBe('output')
  })

  test('minimize sends a panel to the back', () => {
    const l = minimize(reveal(OUTPUT_FLOATING, 'output'), 'output')
    expect(l.recency[l.recency.length - 1]).toBe('output')
  })

  test('a docked panel cannot be minimized', () => {
    const l = minimize(DEFAULT_LAYOUT, 'editor')
    expect(l).toEqual(DEFAULT_LAYOUT)
  })

  test('the active tab is the one fronted most recently', () => {
    const tabbed = moveToOtherSlot(dock(OUTPUT_FLOATING, 'output'), 'output')
    expect(activeIn(tabbed, 'a', ALL)).toBe('output')
    expect(activeIn(reveal(tabbed, 'editor'), 'a', ALL)).toBe('editor')
  })

  test('a slot with nothing present has no active tab', () => {
    expect(activeIn(OUTPUT_FLOATING, 'b', ALL)).toBeNull()
  })

  test('floating panels stack by recency, based at 4', () => {
    const l = reveal(reveal(DEFAULT_LAYOUT, 'trace'), 'output')
    expect(floatingZ(l, 'output')).toBeGreaterThan(floatingZ(l, 'trace'))
    expect(Math.min(...ALL.map((id) => floatingZ(l, id)))).toBe(4)
  })
})

describe('presence', () => {
  test('a trace that has not been opened is not a tab', () => {
    const tabbed = compact(DEFAULT_LAYOUT)
    expect(tabsIn(tabbed, 'a', NO_TRACE)).toEqual(['editor', 'output'])
    expect(tabsIn(tabbed, 'a', ALL)).toEqual(['editor', 'output', 'trace'])
  })

  test('an absent panel is never the active tab', () => {
    const tabbed = reveal(compact(DEFAULT_LAYOUT), 'trace')
    expect(activeIn(tabbed, 'a', ALL)).toBe('trace')
    expect(activeIn(tabbed, 'a', NO_TRACE)).toBe('output')
  })
})

describe('compact', () => {
  test('everything ends up tabbed in one slot, nothing floating', () => {
    const l = compact(dock(OUTPUT_FLOATING, 'output'))
    checkInvariants(l)
    expect(occupiedSlots(l, ALL)).toEqual(['a'])
    expect(ALL.every((id) => l.placement[id].kind === 'docked')).toBe(true)
  })

  test('it is a projection: the wide layout is untouched', () => {
    const wide = setSplitPercent(dock(OUTPUT_FLOATING, 'output'), 30)
    const before = structuredClone(wide)
    compact(wide)
    expect(wide).toEqual(before)
  })

  // The regression test for the whole compact story: activePane used to be
  // hardcoded as trace > output > source, and this has to reproduce it.
  test('it opens on the same panel the old activePane would have picked', () => {
    // Trace present and not put away -> trace.
    const withTrace = reveal(OUTPUT_FLOATING, 'trace')
    expect(activeIn(compact(withTrace), 'a', ALL)).toBe('trace')

    // Trace put away, output not -> output.
    const traceAway = minimize(withTrace, 'trace')
    expect(activeIn(compact(traceAway), 'a', ALL)).toBe('output')

    // Both put away -> the editor, which can never be put away.
    const bothAway = minimize(traceAway, 'output')
    expect(activeIn(compact(bothAway), 'a', ALL)).toBe('editor')

    // No trace at all, output showing -> output.
    expect(activeIn(compact(OUTPUT_FLOATING), 'a', NO_TRACE)).toBe('output')
  })

  test('a minimized panel stays put away rather than jumping to the front', () => {
    const away = minimize(OUTPUT_FLOATING, 'output')
    const tabbed = compact(away)
    expect(tabbed.recency[tabbed.recency.length - 1]).toBe('output')
    expect(activeIn(tabbed, 'a', NO_TRACE)).toBe('editor')
  })
})
