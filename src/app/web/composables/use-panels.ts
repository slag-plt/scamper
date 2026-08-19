import {
  computed,
  inject,
  type InjectionKey,
  nextTick,
  provide,
  ref,
  watch,
  type ComputedRef,
  type Ref,
} from 'vue'
import {
  activeIn,
  compact,
  DEFAULT_LAYOUT,
  dock as dockIn,
  float as floatIn,
  floatingZ,
  isVisible,
  minimize as minimizeIn,
  moveToOtherSlot as moveIn,
  normalize,
  occupiedSlots,
  PANEL_IDS,
  reveal as revealIn,
  setGeometry as setGeometryIn,
  setSplitPercent as setSplitPercentIn,
  tabsIn,
  type Geometry,
  type PanelId,
  type PanelLayout,
  type Placement,
  type SlotId,
} from '../panel-layout'

/**
 * The IDE's panel layout, as reactive state.
 *
 * All the decisions live in panel-layout.ts as pure functions; this adds the
 * ref, the persistence, and the narrow-pane projection on top.
 *
 * Provided rather than a module singleton, unlike editor-prefs and
 * output-prefs. Those are per-user settings; this is per-IDE-instance session
 * state, and the test suite mounts IdeApp dozens of times -- a singleton would
 * leak one test's arrangement into the next.
 */

const STORAGE_KEY = 'scamper.panels'

/** Where an older build kept each floating window's box, one key per window. */
const LEGACY_GEOMETRY_KEYS: Partial<Record<PanelId, string>> = {
  output: 'scamper.window.output',
  trace: 'scamper.window.trace',
}

/** Bumped only for a change this build cannot read forwards. */
const VERSION = 1

function readLegacyGeometry(key: string): Geometry | null {
  try {
    const raw = localStorage.getItem(key)
    if (raw === null) return null
    const parsed: unknown = JSON.parse(raw)
    if (typeof parsed !== 'object' || parsed === null) return null
    const g = parsed as Partial<Geometry>
    return typeof g.x === 'number' &&
      typeof g.y === 'number' &&
      typeof g.w === 'number' &&
      typeof g.h === 'number'
      ? { x: g.x, y: g.y, w: g.w, h: g.h }
      : null
  } catch {
    return null
  }
}

/**
 * Takes the per-window geometry an older build wrote, folds it into `layout`,
 * and removes those keys.
 *
 * Safe to remove them now that this store is the only thing that reads or
 * writes panel geometry -- PanelFrame goes through setGeometry. Removing is
 * what makes the migration happen once, the same shape as takeLegacyConfig in
 * IdeApp: leave the keys and every reload would drag the arrangement back to
 * wherever the old windows happened to be.
 */
function takeLegacyGeometry(layout: PanelLayout): PanelLayout {
  let took = false
  for (const [id, key] of Object.entries(LEGACY_GEOMETRY_KEYS) as [
    PanelId,
    string,
  ][]) {
    const geometry = readLegacyGeometry(key)
    if (geometry === null) continue
    layout.geometry[id] = geometry
    took = true
  }
  if (took) {
    for (const key of Object.values(LEGACY_GEOMETRY_KEYS)) {
      try {
        localStorage.removeItem(key)
      } catch {
        // Nothing to do; the worst case is the migration runs again.
      }
    }
  }
  return layout
}

function readStoredLayout(): PanelLayout {
  let raw: string | null
  try {
    raw = localStorage.getItem(STORAGE_KEY)
  } catch {
    return { ...DEFAULT_LAYOUT }
  }

  // Nothing stored: a first run under this build, so look for what the last
  // one left behind before falling back to the default.
  if (raw === null) return unminimize(takeLegacyGeometry(normalize(undefined)))

  try {
    const parsed: unknown = JSON.parse(raw)
    const version = (parsed as { version?: unknown }).version
    // A newer build wrote this. Guessing at a shape we do not know is worse
    // than starting clean.
    if (version !== VERSION) return { ...DEFAULT_LAYOUT }
    return unminimize(normalize(parsed))
  } catch {
    return { ...DEFAULT_LAYOUT }
  }
}

/**
 * Clears "put away" on load.
 *
 * Geometry, docked-vs-floating and the split are worth remembering; whether a
 * window happened to be minimized when the tab closed is not. Persisting it
 * means a student who tucked the output away once opens Scamper to a pane with
 * no output in it and no obvious way back -- and the old build, which kept
 * this in a plain ref, never did that.
 */
function unminimize(layout: PanelLayout): PanelLayout {
  for (const id of PANEL_IDS) {
    const p = layout.placement[id]
    if (p.kind === 'floating' && p.minimized) {
      layout.placement[id] = { ...p, minimized: false }
    }
  }
  return layout
}

function writeStoredLayout(layout: PanelLayout): void {
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify({ ...layout, version: VERSION }))
  } catch {
    // The layout still works; it just won't survive a reload.
  }
}

export interface PanelsInput {
  /** True when the pane is too narrow to float anything over the code. */
  isCompact: Ref<boolean>
  /** Which panels exist; the trace only does once something has been stepped. */
  present: Ref<readonly PanelId[]>
}

export interface Panels {
  /** What the person arranged. Persisted; the breakpoint never edits it. */
  readonly layout: Ref<PanelLayout>
  /** What is actually drawn: the layout, or its narrow-pane projection. */
  readonly effective: ComputedRef<PanelLayout>
  /**
   * True when `effective` is the projection rather than the arrangement.
   *
   * Controls that move panels have to hide here: the projection ignores
   * placement, so they would change the stored layout with no visible effect.
   */
  readonly isProjected: ComputedRef<boolean>
  reveal: (id: PanelId) => void
  minimize: (id: PanelId) => void
  float: (id: PanelId) => void
  dock: (id: PanelId) => void
  moveToOtherSlot: (id: PanelId) => void
  setGeometry: (id: PanelId, geometry: Geometry) => void
  setSplitPercent: (percent: number) => void
  /** Whether `id` is on screen rather than in the taskbar. */
  isVisible: (id: PanelId) => boolean
  tabs: (slot: SlotId) => PanelId[]
  active: (slot: SlotId) => PanelId | null
  slots: ComputedRef<SlotId[]>
  z: (id: PanelId) => number
  /** The panels put away, in the order the taskbar should offer them. */
  minimized: ComputedRef<PanelId[]>
  /** Suspends persistence for a gesture; resume writes the settled layout. */
  pausePersist: () => void
  resumePersist: () => void
  /**
   * Puts keyboard focus on whatever now represents `id`.
   *
   * Called by the controls that move a panel, and deliberately not by the
   * automatic ones: a run revealing the output must not snatch the caret out
   * of the editor mid-keystroke.
   */
  focusPanel: (id: PanelId) => void
}

function createPanels(input: PanelsInput): Panels {
  const layout = ref<PanelLayout>(readStoredLayout())

  // The breakpoint is a projection, not a state change: crossing back out to a
  // wide pane restores exactly what was arranged, by construction rather than
  // by the two sides happening to agree.
  const effective = computed(() =>
    input.isCompact.value ? compact(layout.value) : layout.value,
  )

  /*
   * Persistence pauses for the duration of a gesture.
   *
   * A splitter drag replaces the layout on every pointermove, and each
   * replacement would otherwise be a JSON.stringify and a synchronous
   * localStorage write -- a hundred of them per drag. PanelFrame avoids this
   * by only calling setGeometry on pointerup; the splitter cannot, because the
   * panes have to move while you drag them.
   */
  let persistPaused = false
  watch(
    layout,
    (l) => {
      if (!persistPaused) writeStoredLayout(l)
    },
    { deep: true },
  )

  const edit = (f: (l: PanelLayout) => PanelLayout) => {
    layout.value = f(layout.value)
  }

  /**
   * The element that stands for a panel right now.
   *
   * Which one depends on where the panel just went, and every case is a
   * control that exists only in that state -- so after a move the previous one
   * is gone, and without this focus falls to <body>. Minimizing a window is
   * the plainest case: the button that was clicked is the one that disappears.
   */
  function focusTarget(id: PanelId): HTMLElement | null {
    const l = effective.value
    const p = l.placement[id] as Placement | undefined
    // Defensive: a caller that has already lost track of which panel it meant
    // should do nothing rather than throw out of a nextTick, where it surfaces
    // as an unhandled rejection with no stack worth reading.
    if (p === undefined) return null
    const pick = (selector: string) =>
      document.querySelector<HTMLElement>(selector)

    if (!isVisible(l, id)) return pick(`[data-panel-taskbar="${id}"]`)
    if (p.kind === 'floating') return pick(`[data-panel-bar="${id}"]`)
    // Tabbed: its tab. Docked alone: the panel body, which carries tabindex 0
    // only in a tab group, so fall through to the frame either way.
    return pick(`#tab-${id}`) ?? pick(`#panel-${id}`)
  }

  return {
    layout,
    effective,
    isProjected: computed(() => input.isCompact.value),
    pausePersist: () => { persistPaused = true },
    resumePersist: () => {
      persistPaused = false
      writeStoredLayout(layout.value)
    },
    focusPanel: (id) => {
      // After the DOM has caught up: the target usually does not exist yet.
      void nextTick(() => focusTarget(id)?.focus())
    },
    reveal: (id) => { edit((l) => revealIn(l, id)) },
    minimize: (id) => { edit((l) => minimizeIn(l, id)) },
    float: (id) => { edit((l) => floatIn(l, id)) },
    dock: (id) => { edit((l) => dockIn(l, id)) },
    moveToOtherSlot: (id) => { edit((l) => moveIn(l, id)) },
    setGeometry: (id, geometry) => { edit((l) => setGeometryIn(l, id, geometry)) },
    setSplitPercent: (percent) => { edit((l) => setSplitPercentIn(l, percent)) },
    isVisible: (id) => isVisible(effective.value, id),
    tabs: (slot) => tabsIn(effective.value, slot, input.present.value),
    active: (slot) => activeIn(effective.value, slot, input.present.value),
    slots: computed(() => occupiedSlots(effective.value, input.present.value)),
    z: (id) => floatingZ(effective.value, id),
    minimized: computed(() =>
      // Recency order, so the taskbar lists them the way they were put away.
      effective.value.recency.filter(
        (id) => input.present.value.includes(id) && !isVisible(effective.value, id),
      ),
    ),
  }
}

const PanelsKey: InjectionKey<Panels> = Symbol('Panels')

export function providePanels(input: PanelsInput): Panels {
  const panels = createPanels(input)
  provide(PanelsKey, panels)
  return panels
}

export function usePanels(): Panels {
  const panels = inject(PanelsKey)
  if (!panels) {
    throw new Error('Panels missing: call providePanels() in an ancestor')
  }
  return panels
}
