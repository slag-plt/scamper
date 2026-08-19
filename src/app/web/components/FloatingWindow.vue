<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'

/**
 * A window that floats over whatever fills the pane behind it: draggable by its
 * title bar, resizable from any edge or corner, and minimizable to a taskbar
 * the parent renders.
 *
 * It positions itself absolutely inside its parent, so the parent must be
 * positioned and is the box the window is kept inside. Geometry is remembered
 * under `storageKey`, because a window that forgets where it was put on every
 * reload is a window nobody moves twice.
 */
const props = defineProps<{
  title: string
  /** localStorage key for this window's remembered geometry. */
  storageKey: string
  /**
   * Fills the parent instead of floating in it, dropping the title bar and the
   * resize handles with it. This is what a pane too narrow to float a window in
   * uses; the parent shows the window and whatever is behind it in turn, so the
   * chrome for moving it would have nowhere to move it to.
   */
  docked?: boolean
}>()

const minimized = defineModel<boolean>('minimized', { default: false })

/** Small enough to tuck away, large enough to still be a window. */
const MIN_WIDTH = 240
const MIN_HEIGHT = 120

interface Geometry {
  x: number
  y: number
  w: number
  h: number
}

const root = ref<HTMLElement | null>(null)
// Provisional until `mounted` measures the parent; see `applyDefaultGeometry`.
const geometry = ref<Geometry>({ x: 0, y: 0, w: 420, h: 320 })
const isDragging = ref(false)

const style = computed(() => ({
  left: `${String(geometry.value.x)}px`,
  top: `${String(geometry.value.y)}px`,
  width: `${String(geometry.value.w)}px`,
  height: `${String(geometry.value.h)}px`,
}))

// ---------- remembering where it was put ----------

function readStored(): Geometry | null {
  try {
    const raw = localStorage.getItem(props.storageKey)
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
    return null // no storage, or something else wrote nonsense here
  }
}

function store() {
  try {
    localStorage.setItem(props.storageKey, JSON.stringify(geometry.value))
  } catch {
    // The window still works; it just won't remember this position.
  }
}

// ---------- staying inside the parent ----------

/** @returns the parent's content box, or null before mount. */
function bounds(): { w: number; h: number } | null {
  const parent = root.value?.parentElement
  return parent === null || parent === undefined
    ? null
    : { w: parent.clientWidth, h: parent.clientHeight }
}

/**
 * Pulls `g` back inside the parent. Sizes are capped before positions so a
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

/** Opens bottom-right of the pane, over the end of the code rather than its start. */
function applyDefaultGeometry() {
  const b = bounds()
  if (b === null) return
  const w = Math.max(MIN_WIDTH, Math.round(b.w * 0.42))
  const h = Math.max(MIN_HEIGHT, Math.round(b.h * 0.62))
  geometry.value = clamp({ x: b.w - w - 16, y: b.h - h - 16, w, h })
}

// ---------- dragging and resizing ----------

/** Which edges a resize is pulling; dragging the title bar moves all four. */
type Direction = 'n' | 's' | 'e' | 'w' | 'ne' | 'nw' | 'se' | 'sw'

const RESIZE_DIRECTIONS: Direction[] = ['n', 's', 'e', 'w', 'ne', 'nw', 'se', 'sw']

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
  target.setPointerCapture(event.pointerId)

  const onMove = (e: PointerEvent) => {
    geometry.value = clamp(update(e.clientX - startX, e.clientY - startY, start))
  }
  const onUp = () => {
    isDragging.value = false
    target.releasePointerCapture(event.pointerId)
    target.removeEventListener('pointermove', onMove)
    target.removeEventListener('pointerup', onUp)
    target.removeEventListener('pointercancel', onUp)
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
  const stored = readStored()
  if (stored === null) {
    applyDefaultGeometry()
  } else {
    geometry.value = clamp(stored)
  }
  // Resizing the browser, or opening the file drawer, shrinks the pane under
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

// Restoring from the taskbar re-measures, since the pane may have changed
// shape while the window was away.
watch(minimized, (isMin) => {
  if (!isMin) geometry.value = clamp(geometry.value)
})
</script>

<template>
  <!-- A labelled region, not a dialog: it is non-modal, always present, and
       traps no focus, so calling it a dialog would both mislead a screen
       reader and collide with the app's real modals. -->
  <section
    v-show="!minimized"
    ref="root"
    class="floating-window"
    :class="{ dragging: isDragging, docked }"
    :style="docked ? undefined : style"
    :aria-label="title"
  >
    <div v-if="!docked" class="window-bar" @pointerdown="startMove">
      <span class="window-title">{{ title }}</span>
      <button
        type="button"
        class="window-button fa-solid fa-minus"
        :title="`Minimize ${title}`"
        :aria-label="`Minimize ${title}`"
        @pointerdown.stop
        @click="minimized = true"
      ></button>
    </div>
    <div class="window-body">
      <slot></slot>
    </div>
    <template v-if="!docked">
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
.floating-window {
  position: absolute;
  z-index: 4;
  display: flex;
  flex-direction: column;
  background: var(--surface);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: 6px;
  box-shadow: 0 8px 28px rgba(0, 0, 0, 0.3);
  overflow: hidden;
}

/* Filling the pane rather than floating in it: no corners, no shadow, and no
   border, since there is nothing beside it to be separated from. */
.floating-window.docked {
  inset: 0;
  width: auto;
  height: auto;
  border: none;
  border-radius: 0;
  box-shadow: none;
}

/* Whatever is inside must not swallow the pointer mid-gesture -- a drag that
   passes over the output would otherwise start selecting its text. */
.floating-window.dragging {
  user-select: none;
}

.floating-window.dragging .window-body {
  pointer-events: none;
}

.window-bar {
  display: flex;
  align-items: center;
  gap: 0.5em;
  padding: 0.25em 0.35em 0.25em 0.65em;
  background: var(--header-bg);
  color: var(--header-fg);
  border-bottom: 1px solid var(--border);
  font-size: 0.85rem;
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
  padding: 0.2em 0.45em;
  font-size: 0.8em;
  line-height: 1;
  color: inherit;
  border-radius: 3px;
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
