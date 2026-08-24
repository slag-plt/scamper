/**
 * Where the IDE's panels are.
 *
 * The editor, the output and the step trace are the same kind of thing: a
 * panel, which is either floating over the pane or docked into one of two
 * slots. "Tabbed" is not a third case -- a panel is tabbed when it shares a
 * slot with another, which is a fact about the slot. Making it its own case
 * would let the two disagree.
 *
 * Everything here is pure data and pure functions over it. The reactive
 * wrapper, the persistence and the DOM all live elsewhere (see
 * composables/use-panels.ts), which is what lets this be unit-tested under
 * jsdom -- a place with no layout at all.
 */

/** The surfaces the IDE can show. Closed on purpose: there are these three. */
export type PanelId = 'editor' | 'output' | 'trace'

/** Canonical order. It decides the order tabs appear in, and nothing else. */
export const PANEL_IDS = ['editor', 'output', 'trace'] as const

/** The dock's two slots, in reading order. */
export type SlotId = 'a' | 'b'

const SLOT_IDS = ['a', 'b'] as const

/** A floating panel's box, in pixels relative to the dock. */
export interface Geometry {
  x: number
  y: number
  w: number
  h: number
}

/** Where one panel is. */
export type Placement =
  | { kind: 'docked'; slot: SlotId }
  | { kind: 'floating'; minimized: boolean }

export type SplitAxis = 'row' | 'column'

export interface PanelLayout {
  /** Total over PanelId, so no panel can go missing or be in two places. */
  placement: Record<PanelId, Placement>
  /**
   * Where each panel floats, kept apart from its placement on purpose.
   *
   * A window's box is a property of the window, not of where it happens to be
   * right now. Holding it inside the floating placement meant docking a window
   * threw it away, so floating it again put it back at the default position
   * rather than where its owner had left it. `null` means "never placed"; the
   * frame measures a default on mount.
   */
  geometry: Record<PanelId, Geometry | null>
  /**
   * Every panel, most recently brought forward first.
   *
   * One array answers three questions that would otherwise be three fields
   * drifting apart: which tab a slot shows, which floating panel is on top,
   * and which panel a narrow pane opens on.
   */
  recency: PanelId[]
  /** Which way the dock splits, and how much of it slot A takes. */
  axis: SplitAxis
  splitPercent: number
}

/** Slot A takes this share of the dock until someone drags the splitter. */
const DEFAULT_SPLIT = 62
const MIN_SPLIT = 15
const MAX_SPLIT = 85

/**
 * The arrangement a new user gets: code and output docked side by side, with
 * the trace floating over them.
 *
 * The output starts docked rather than floating (#371). A window volunteering
 * itself over the code reads as something gone wrong to a beginner, where a
 * pane beside it reads as part of the editor -- which is also where the IDE
 * put it before the dock existed.
 *
 * The trace still floats: it is transient and opened on demand, so it arrives
 * as the answer to something the person just asked for, and taking a third of
 * the dock permanently for it would not pay.
 *
 * The recency order is trace, output, editor -- the reverse of the canonical
 * one. Nothing has been fronted yet at startup, so this is the tie-break, and
 * it is chosen to reproduce the precedence the IDE has always used on a narrow
 * pane: the trace if it is open, otherwise the output, otherwise the code.
 */
export const DEFAULT_LAYOUT: PanelLayout = {
  placement: {
    editor: { kind: 'docked', slot: 'a' },
    output: { kind: 'docked', slot: 'b' },
    trace: { kind: 'floating', minimized: false },
  },
  geometry: { editor: null, output: null, trace: null },
  recency: ['trace', 'output', 'editor'],
  axis: 'row',
  splitPercent: DEFAULT_SPLIT,
}

// ---------- helpers ----------

function clone(l: PanelLayout): PanelLayout {
  return {
    placement: { ...l.placement },
    geometry: { ...l.geometry },
    recency: [...l.recency],
    axis: l.axis,
    splitPercent: l.splitPercent,
  }
}

function slotOf(l: PanelLayout, id: PanelId): SlotId | null {
  const p = l.placement[id]
  return p.kind === 'docked' ? p.slot : null
}

/** @returns the panels docked in `slot`, in canonical order. */
function dockedIn(l: PanelLayout, slot: SlotId): PanelId[] {
  return PANEL_IDS.filter((id) => slotOf(l, id) === slot)
}

// ---------- invariants ----------

function isGeometry(v: unknown): v is Geometry {
  if (typeof v !== 'object' || v === null) return false
  const g = v as Record<string, unknown>
  return (
    typeof g.x === 'number' &&
    typeof g.y === 'number' &&
    typeof g.w === 'number' &&
    typeof g.h === 'number'
  )
}

function normalizePlacement(v: unknown, fallback: Placement): Placement {
  if (typeof v !== 'object' || v === null) return fallback
  const p = v as Record<string, unknown>
  if (p.kind === 'docked') {
    const slot = SLOT_IDS.find((s) => s === p.slot)
    return slot === undefined ? fallback : { kind: 'docked', slot }
  }
  if (p.kind === 'floating') {
    return { kind: 'floating', minimized: p.minimized === true }
  }
  return fallback
}

/**
 * The one place a layout's invariants are established.
 *
 * Every verb below ends here, and so does everything read out of storage, so
 * no caller -- and no older build's leftovers -- can produce an illegal one.
 *
 * @param value anything at all, including a parsed blob of unknown vintage
 */
export function normalize(value: unknown): PanelLayout {
  const raw = (typeof value === 'object' && value !== null ? value : {}) as
    Record<string, unknown>

  const rawPlacement = (
    typeof raw.placement === 'object' && raw.placement !== null
      ? raw.placement
      : {}
  ) as Record<string, unknown>

  const placement = Object.fromEntries(
    PANEL_IDS.map((id) => [
      id,
      normalizePlacement(rawPlacement[id], DEFAULT_LAYOUT.placement[id]),
    ]),
  ) as Record<PanelId, Placement>

  // A permutation of PANEL_IDS: drop what we don't know, drop repeats, and
  // append whatever is missing in canonical order.
  const seen = new Set<PanelId>()
  const recency: PanelId[] = []
  if (Array.isArray(raw.recency)) {
    for (const id of raw.recency as unknown[]) {
      const known = PANEL_IDS.find((p) => p === id)
      if (known !== undefined && !seen.has(known)) {
        seen.add(known)
        recency.push(known)
      }
    }
  }
  // Whatever is missing is appended in the default's order, not the canonical
  // one: that order is the startup tie-break, and normalize is where a layout
  // with no recency at all (a first run, or a corrupted blob) gets one.
  for (const id of DEFAULT_LAYOUT.recency) if (!seen.has(id)) recency.push(id)

  const percent = typeof raw.splitPercent === 'number' && !Number.isNaN(raw.splitPercent)
    ? Math.min(MAX_SPLIT, Math.max(MIN_SPLIT, raw.splitPercent))
    : DEFAULT_SPLIT

  const rawGeometry = (
    typeof raw.geometry === 'object' && raw.geometry !== null ? raw.geometry : {}
  ) as Record<string, unknown>

  const geometry = Object.fromEntries(
    PANEL_IDS.map((id) => {
      const g = rawGeometry[id]
      return [id, isGeometry(g) ? g : null]
    }),
  ) as Record<PanelId, Geometry | null>

  const layout: PanelLayout = {
    placement,
    geometry,
    recency,
    axis: raw.axis === 'column' ? 'column' : 'row',
    splitPercent: percent,
  }

  // Canonicalize an empty A with an occupied B into an occupied A. Without
  // this the dock would have to render a third arrangement -- a gap where a
  // slot used to be -- that means exactly the same thing as one slot.
  if (dockedIn(layout, 'a').length === 0) {
    for (const id of dockedIn(layout, 'b')) {
      layout.placement[id] = { kind: 'docked', slot: 'a' }
    }
  }

  return layout
}

// ---------- verbs ----------

/** Brings `id` forward: to the front of the stack, or to its slot's front tab. */
export function reveal(l: PanelLayout, id: PanelId): PanelLayout {
  const next = clone(l)
  const p = next.placement[id]
  if (p.kind === 'floating' && p.minimized) {
    next.placement[id] = { ...p, minimized: false }
  }
  next.recency = [id, ...next.recency.filter((p2) => p2 !== id)]
  return normalize(next)
}

/** Puts `id` away. A docked panel cannot be minimized, only a floating one. */
export function minimize(l: PanelLayout, id: PanelId): PanelLayout {
  const p = l.placement[id]
  if (p.kind !== 'floating') return l
  const next = clone(l)
  next.placement[id] = { ...p, minimized: true }
  // To the back, so whatever is left takes the front.
  next.recency = [...next.recency.filter((p2) => p2 !== id), id]
  return normalize(next)
}

/**
 * Pops `id` out of the dock, back to wherever it last floated.
 *
 * Its geometry is untouched, so a window that is docked and floated again
 * returns to the box its owner left it in rather than to the default corner.
 */
export function float(l: PanelLayout, id: PanelId): PanelLayout {
  if (l.placement[id].kind === 'floating') return l
  const next = clone(l)
  next.placement[id] = { kind: 'floating', minimized: false }
  next.recency = [id, ...next.recency.filter((p) => p !== id)]
  return normalize(next)
}

/**
 * Docks `id`, always into slot B -- "put it on the far side".
 *
 * Completely predictable, and normalize() collapses it back to A when there is
 * nothing else docked, so there is no heuristic about which side is emptier.
 */
export function dock(l: PanelLayout, id: PanelId): PanelLayout {
  if (l.placement[id].kind === 'docked') return l
  const next = clone(l)
  next.placement[id] = { kind: 'docked', slot: 'b' }
  next.recency = [id, ...next.recency.filter((p) => p !== id)]
  return normalize(next)
}

/**
 * Sends a docked panel to the other slot.
 *
 * The single command that expresses split <-> tabbed in both directions: move
 * the lone occupant of B into A and the two become tabs; move one of A's tabs
 * into B and they become a split.
 */
export function moveToOtherSlot(l: PanelLayout, id: PanelId): PanelLayout {
  const slot = slotOf(l, id)
  if (slot === null) return l
  const next = clone(l)
  next.placement[id] = { kind: 'docked', slot: slot === 'a' ? 'b' : 'a' }
  next.recency = [id, ...next.recency.filter((p) => p !== id)]
  return normalize(next)
}

/** Records where a floating panel has been dragged or resized to. */
export function setGeometry(l: PanelLayout, id: PanelId, geometry: Geometry): PanelLayout {
  const next = clone(l)
  next.geometry[id] = geometry
  return next
}

/** Moves the splitter. Clamped, so a slot can never be dragged shut. */
export function setSplitPercent(l: PanelLayout, percent: number): PanelLayout {
  const next = clone(l)
  next.splitPercent = percent
  return normalize(next)
}

export function setAxis(l: PanelLayout, axis: SplitAxis): PanelLayout {
  const next = clone(l)
  next.axis = axis
  return normalize(next)
}

// ---------- selectors ----------

/** Whether `id` is on screen rather than tucked away in the taskbar. */
export function isVisible(l: PanelLayout, id: PanelId): boolean {
  const p = l.placement[id]
  return p.kind === 'docked' || !p.minimized
}

/**
 * The tabs `slot` shows, in canonical order.
 *
 * @param present the panels that exist right now; the trace only does once
 *                something has been stepped
 */
export function tabsIn(
  l: PanelLayout,
  slot: SlotId,
  present: readonly PanelId[],
): PanelId[] {
  return dockedIn(l, slot).filter((id) => present.includes(id))
}

/** The tab `slot` is showing: whichever of its panels was fronted last. */
export function activeIn(
  l: PanelLayout,
  slot: SlotId,
  present: readonly PanelId[],
): PanelId | null {
  const tabs = tabsIn(l, slot, present)
  return l.recency.find((id) => tabs.includes(id)) ?? null
}

/** The slots holding at least one present panel, in reading order. */
export function occupiedSlots(
  l: PanelLayout,
  present: readonly PanelId[],
): SlotId[] {
  return SLOT_IDS.filter((s) => tabsIn(l, s, present).length > 0)
}

/**
 * A floating panel's z-index.
 *
 * Based at 4, which is where floating windows have always sat relative to the
 * menu bar (3) and the header (2). Ordering them by recency is what gives
 * click-to-raise; before this every window shared one value and the later one
 * in DOM order always won.
 */
export function floatingZ(l: PanelLayout, id: PanelId): number {
  const at = l.recency.indexOf(id)
  return 4 + (PANEL_IDS.length - 1 - (at < 0 ? PANEL_IDS.length - 1 : at))
}

/**
 * The layout as a pane too narrow to float anything can draw it: everything
 * tabbed into one slot.
 *
 * A projection rather than an edit, so crossing back out to a wide pane
 * restores exactly what the person had arranged. Panels that were put away
 * fall to the back of the recency order, which is how a window someone
 * minimized stays put away as a tab rather than jumping to the front.
 */
export function compact(l: PanelLayout): PanelLayout {
  const shown = l.recency.filter((id) => isVisible(l, id))
  const hidden = l.recency.filter((id) => !isVisible(l, id))
  return {
    ...l,
    placement: {
      editor: { kind: 'docked', slot: 'a' },
      output: { kind: 'docked', slot: 'a' },
      trace: { kind: 'docked', slot: 'a' },
    },
    recency: [...shown, ...hidden],
  }
}
