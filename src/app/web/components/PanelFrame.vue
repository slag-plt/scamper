<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import { usePanels } from '../composables/use-panels'
import { PANEL_IDS, type Geometry, type PanelId } from '../panel-layout'

/**
 * One panel's chrome.
 *
 * Floating, it is a window: draggable by its title bar, resizable from any
 * edge or corner, minimizable to the taskbar the dock renders. Docked, it is a
 * region of the dock's grid, with no chrome of its own -- either filling it, or
 * sharing a slot with others behind a tab strip.
 *
 * The two are the same element with different classes, deliberately. Moving a
 * panel between branches of a template would remount whatever is inside it,
 * and what is inside is a CodeMirror editor holding a document and an undo
 * history, or an output pane bound to a run in flight. Neither survives that.
 * So: one place in the tree, and only the CSS changes.
 */
const props = defineProps<{
  id: PanelId
  title: string
  /** Adds a close button beside minimize, and a close affordance on its tab. */
  closeable?: boolean
}>()

const emit = defineEmits<{ close: [] }>()

const panels = usePanels()

/** Small enough to tuck away, large enough to still be a window. */
const MIN_WIDTH = 240
const MIN_HEIGHT = 120

const root = ref<HTMLElement | null>(null)
// Provisional until `mounted` measures the dock; see `applyDefaultGeometry`.
const geometry = ref<Geometry>({ x: 0, y: 0, w: 420, h: 320 })
const isDragging = ref(false)

const placement = computed(() => panels.effective.value.placement[props.id])
const isFloating = computed(() => placement.value.kind === 'floating')

/**
 * Whether the panel is drawn at all.
 *
 * Floating, that means it has not been minimized. Docked, it means it is the
 * tab its slot is showing -- the others are still mounted, just not on screen.
 */
const isShown = computed(() => {
  const p = placement.value
  if (p.kind === 'floating') return !p.minimized
  return panels.active(p.slot) === props.id
})

/** Which slot's grid area to occupy; null while floating. */
const slot = computed(() =>
  placement.value.kind === 'docked' ? placement.value.slot : null,
)

/**
 * Whether this panel is one of several tabs, which decides how it is announced.
 *
 * In a tab group it is a tabpanel, named by its tab. Otherwise -- floating, or
 * docked on its own -- it is a labelled region, which is what it has always
 * been. The reasoning for a region rather than a dialog is in the template.
 */
const isTabbed = computed(() => {
  const p = placement.value
  return p.kind === 'docked' && panels.tabs(p.slot).length > 1
})

/*
 * Derived ids rather than useId(): there is exactly one dock on the page (the
 * IDE holds a single-instance lock), so they cannot collide, and a tab in
 * PanelTabs can name its panel without a registry to look it up in.
 */
const panelElementId = computed(() => `panel-${props.id}`)

const style = computed(() =>
  isFloating.value
    ? {
        left: `${String(geometry.value.x)}px`,
        top: `${String(geometry.value.y)}px`,
        width: `${String(geometry.value.w)}px`,
        height: `${String(geometry.value.h)}px`,
        zIndex: String(panels.z(props.id)),
      }
    : undefined,
)

/**
 * Whether this panel draws its own title bar.
 *
 * Only when floating. A docked panel is labelled by its tab, or -- when it is
 * the only thing in the dock -- needs no label at all, which is what keeps the
 * default arrangement looking exactly as it did before the dock existed.
 */
const hasBar = computed(() => isFloating.value)

function dockSelf() {
  panels.dock(props.id)
  panels.focusPanel(props.id)
}

function minimizeSelf() {
  panels.minimize(props.id)
  panels.focusPanel(props.id)
}

// ---------- staying inside the dock ----------

/** @returns the dock's content box, or null before mount. */
function bounds(): { w: number; h: number } | null {
  const parent = root.value?.parentElement
  return parent === null || parent === undefined
    ? null
    : { w: parent.clientWidth, h: parent.clientHeight }
}

/**
 * Pulls `g` back inside the dock. Sizes are capped before positions so a
 * window larger than a shrunken pane ends up filling it rather than hanging
 * off the edge.
 */
function clamp(g: Geometry): Geometry {
  const b = bounds()
  if (b === null) return g
  const w = Math.max(MIN_WIDTH, Math.min(g.w, b.w))
  const h = Math.max(MIN_HEIGHT, Math.min(g.h, b.h))
  return {
    w,
    h,
    x: Math.max(0, Math.min(g.x, b.w - w)),
    y: Math.max(0, Math.min(g.y, b.h - h)),
  }
}

/**
 * Opens bottom-right of the pane, over the end of the code rather than its
 * start, stepped back by one place per floating panel ahead of it.
 *
 * Without the step every window with no remembered position opens at exactly
 * the same inset, so a second one lands precisely on the first and looks like
 * the first simply changed.
 */
function applyDefaultGeometry() {
  const b = bounds()
  if (b === null) return
  const w = Math.max(MIN_WIDTH, Math.round(b.w * 0.42))
  const h = Math.max(MIN_HEIGHT, Math.round(b.h * 0.62))
  const inset = 16 + 24 * cascadeIndex()
  geometry.value = clamp({ x: b.w - w - inset, y: b.h - h - inset, w, h })
}

/** This panel's place among the floating ones, in canonical order. */
function cascadeIndex(): number {
  const layout = panels.layout.value
  const floating = PANEL_IDS.filter(
    (id) => layout.placement[id].kind === 'floating',
  )
  return Math.max(0, floating.indexOf(props.id))
}

/** Hands the settled box to the store, which is what persists it. */
function store() {
  panels.setGeometry(props.id, geometry.value)
}

// ---------- dragging and resizing ----------

/** Which edges a resize is pulling; dragging the title bar moves all four. */
type Direction = 'n' | 's' | 'e' | 'w' | 'ne' | 'nw' | 'se' | 'sw'

const RESIZE_DIRECTIONS: Direction[] = ['n', 's', 'e', 'w', 'ne', 'nw', 'se', 'sw']

/**
 * Pointer capture, best-effort.
 *
 * setPointerCapture throws NotFoundError when the pointer id is no longer
 * active -- a fast flick that ends before the handler runs, or an event
 * synthesised by an assistive tool or a test. Losing the whole gesture over it
 * is worse than dragging without capture, which still works as long as the
 * pointer stays over the element.
 */
function capturePointer(el: HTMLElement, id: number) {
  try {
    el.setPointerCapture(id)
  } catch {
    // Drag on regardless.
  }
}

function releasePointer(el: HTMLElement, id: number) {
  try {
    el.releasePointerCapture(id)
  } catch {
    // Already released, or never captured.
  }
}

/**
 * Runs a pointer drag, feeding each move to `update` as a delta from where the
 * press started. Pointer capture keeps the gesture alive over the editor
 * behind, and past the edge of the window itself.
 */
function trackPointer(
  event: PointerEvent,
  update: (dx: number, dy: number, start: Geometry) => Geometry,
) {
  event.preventDefault()
  const target = event.currentTarget as HTMLElement
  const startX = event.clientX
  const startY = event.clientY
  const start = { ...geometry.value }
  isDragging.value = true
  capturePointer(target, event.pointerId)

  const onMove = (e: PointerEvent) => {
    geometry.value = clamp(update(e.clientX - startX, e.clientY - startY, start))
  }
  const onUp = () => {
    isDragging.value = false
    releasePointer(target, event.pointerId)
    target.removeEventListener('pointermove', onMove)
    target.removeEventListener('pointerup', onUp)
    target.removeEventListener('pointercancel', onUp)
    // Only on gesture end: writing every pointermove would persist a hundred
    // layouts per drag.
    store()
  }
  target.addEventListener('pointermove', onMove)
  target.addEventListener('pointerup', onUp)
  target.addEventListener('pointercancel', onUp)
}

function startMove(event: PointerEvent) {
  trackPointer(event, (dx, dy, start) => ({
    ...start,
    x: start.x + dx,
    y: start.y + dy,
  }))
}

function startResize(event: PointerEvent, dir: Direction) {
  trackPointer(event, (dx, dy, start) => {
    const g = { ...start }
    // Pulling a top or left edge moves the window as well as sizing it, and
    // stops at the minimum rather than letting the far edge run away.
    if (dir.includes('e')) g.w = Math.max(MIN_WIDTH, start.w + dx)
    if (dir.includes('s')) g.h = Math.max(MIN_HEIGHT, start.h + dy)
    if (dir.includes('w')) {
      g.w = Math.max(MIN_WIDTH, start.w - dx)
      g.x = start.x + (start.w - g.w)
    }
    if (dir.includes('n')) {
      g.h = Math.max(MIN_HEIGHT, start.h - dy)
      g.y = start.y + (start.h - g.h)
    }
    return g
  })
}

// ---------- lifecycle ----------

let observer: ResizeObserver | null = null

onMounted(() => {
  const stored = panels.layout.value.geometry[props.id]
  if (stored === null) {
    applyDefaultGeometry()
    store()
  } else {
    geometry.value = clamp(stored)
  }
  // Resizing the browser, or opening the file drawer, shrinks the dock under
  // the window; without this it would be left hanging outside it.
  const parent = root.value?.parentElement
  if (parent) {
    observer = new ResizeObserver(() => {
      geometry.value = clamp(geometry.value)
    })
    observer.observe(parent)
  }
})

onUnmounted(() => {
  observer?.disconnect()
})

// Coming back from the taskbar, or back from being docked, re-measures: the
// pane may have changed shape while the window was away.
watch(isShown, (shown) => {
  if (shown) geometry.value = clamp(geometry.value)
})
</script>

<template>
  <!-- A labelled region, not a dialog: it is non-modal, always present, and
       traps no focus, so calling it a dialog would both mislead a screen
       reader and collide with the app's real modals. -->
  <section
    v-show="isShown"
    ref="root"
    class="panel-frame"
    :class="[
      isFloating ? 'floating' : 'docked',
      slot === null ? null : `in-${slot}`,
      { dragging: isDragging },
    ]"
    :style="style"
    :data-panel="id"
    :data-placement="isFloating ? 'floating' : 'docked'"
    :data-slot="slot ?? undefined"
    :id="panelElementId"
    :role="isTabbed ? 'tabpanel' : undefined"
    :aria-labelledby="isTabbed ? `tab-${id}` : undefined"
    :aria-label="isTabbed ? undefined : title"
    :tabindex="isTabbed ? 0 : undefined"
    @pointerdown="isFloating && panels.reveal(id)"
  >
    <div
      v-if="hasBar"
      class="window-bar"
      :data-panel-bar="id"
      tabindex="-1"
      @pointerdown="startMove"
    >
      <span class="window-title">{{ title }}</span>
      <button
        type="button"
        class="window-button fa-solid fa-table-columns"
        :title="`Dock ${title}`"
        :aria-label="`Dock ${title}`"
        @pointerdown.stop
        @click="dockSelf"
      ></button>
      <button
        type="button"
        class="window-button fa-solid fa-minus"
        :title="`Minimize ${title}`"
        :aria-label="`Minimize ${title}`"
        @pointerdown.stop
        @click="minimizeSelf"
      ></button>
      <button
        v-if="closeable"
        type="button"
        class="window-button fa-solid fa-xmark"
        :title="`Close ${title}`"
        :aria-label="`Close ${title}`"
        @pointerdown.stop
        @click="emit('close')"
      ></button>
    </div>
    <div class="window-body">
      <slot></slot>
    </div>
    <template v-if="isFloating">
      <div
        v-for="dir in RESIZE_DIRECTIONS"
        :key="dir"
        class="resize-handle"
        :class="`resize-${dir}`"
        @pointerdown="startResize($event, dir)"
      ></div>
    </template>
  </section>
</template>

<style scoped>
.panel-frame {
  display: flex;
  flex-direction: column;
  min-width: 0;
  min-height: 0;
  background: var(--surface);
  color: var(--fg);
  overflow: hidden;
}

.panel-frame.floating {
  position: absolute;
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-lg);
}

/*
 * Docked panels are grid items of the dock, placed by slot.
 *
 * Note there is no `grid-area` on the floating variant, and that is
 * load-bearing rather than an omission: an absolutely positioned grid child
 * that has one takes that *area* as its containing block instead of the grid's
 * padding box, which would quietly break every clamp() above.
 */
.panel-frame.in-a {
  grid-area: body-a;
}

.panel-frame.in-b {
  grid-area: body-b;
}

/* Whatever is inside must not swallow the pointer mid-gesture -- a drag that
   passes over the output would otherwise start selecting its text. */
.panel-frame.dragging {
  user-select: none;
}

.panel-frame.dragging .window-body {
  pointer-events: none;
}

.window-bar {
  display: flex;
  align-items: center;
  gap: var(--space-md);
  padding: var(--space-xs) var(--space-sm) var(--space-xs) var(--space-lg);
  background: var(--header-bg);
  color: var(--header-fg);
  border-bottom: 1px solid var(--border);
  font-size: var(--text-md);
  cursor: move;
  user-select: none;
  flex-shrink: 0;
}

.window-title {
  flex: 1;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-weight: 600;
}

.window-button {
  flex-shrink: 0;
  border: none;
  background: none;
  padding: var(--space-2xs) var(--space-sm);
  font-size: var(--text-xs);
  line-height: 1;
  color: inherit;
  border-radius: var(--radius-sm);
  cursor: pointer;
}

.window-button:hover {
  background: color-mix(in srgb, currentColor 22%, transparent);
}

.window-body {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

/* Eight grab zones straddling the border, wide enough to hit without being
   wide enough to steal clicks from the content just inside them. */
.resize-handle {
  position: absolute;
}

.resize-n,
.resize-s {
  left: 8px;
  right: 8px;
  height: 6px;
  cursor: ns-resize;
}
.resize-n {
  top: -3px;
}
.resize-s {
  bottom: -3px;
}

.resize-e,
.resize-w {
  top: 8px;
  bottom: 8px;
  width: 6px;
  cursor: ew-resize;
}
.resize-e {
  right: -3px;
}
.resize-w {
  left: -3px;
}

.resize-ne,
.resize-nw,
.resize-se,
.resize-sw {
  width: 12px;
  height: 12px;
}
.resize-ne {
  top: -3px;
  right: -3px;
  cursor: nesw-resize;
}
.resize-nw {
  top: -3px;
  left: -3px;
  cursor: nwse-resize;
}
.resize-se {
  bottom: -3px;
  right: -3px;
  cursor: nwse-resize;
}
.resize-sw {
  bottom: -3px;
  left: -3px;
  cursor: nesw-resize;
}
</style>
