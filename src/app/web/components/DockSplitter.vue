<script setup lang="ts">
import { computed } from 'vue'
import { usePanels } from '../composables/use-panels'

/**
 * The draggable divider between the dock's two slots.
 *
 * Hand-rolled rather than reached for from splitpanes, and the reason is
 * structural: splitpanes owns its children's DOM, wrapping each in a `<Pane>`
 * it sizes with inline styles. A panel inside one of those cannot also be an
 * absolutely positioned overlay from the same place in the tree, so adopting it
 * would force a `v-if` branch or a Teleport -- and either remounts the panel,
 * which is exactly what PanelFrame exists to avoid. It also has no keyboard
 * resize, which is half of what is below.
 */
const panels = usePanels()

const NUDGE = 2
const MIN = 15
const MAX = 85

const percent = computed(() => panels.effective.value.splitPercent)
const isColumn = computed(() => panels.effective.value.axis === 'column')

/**
 * Turns a pointer position into a percentage of the dock.
 *
 * Measured against the dock rather than accumulated from deltas, so the
 * divider tracks the pointer exactly instead of drifting once it hits a clamp.
 */
function percentAt(e: PointerEvent, dock: HTMLElement): number {
  const box = dock.getBoundingClientRect()
  const along = isColumn.value
    ? (e.clientY - box.top) / box.height
    : (e.clientX - box.left) / box.width
  return along * 100
}

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

function onPointerDown(event: PointerEvent) {
  const target = event.currentTarget as HTMLElement
  const dock = target.parentElement
  if (dock === null) return
  event.preventDefault()
  capturePointer(target, event.pointerId)
  panels.pausePersist()

  const onMove = (e: PointerEvent) => {
    panels.setSplitPercent(percentAt(e, dock))
  }
  const onUp = () => {
    panels.resumePersist()
    releasePointer(target, event.pointerId)
    target.removeEventListener('pointermove', onMove)
    target.removeEventListener('pointerup', onUp)
    target.removeEventListener('pointercancel', onUp)
  }
  target.addEventListener('pointermove', onMove)
  target.addEventListener('pointerup', onUp)
  target.addEventListener('pointercancel', onUp)
}

function onKeyDown(event: KeyboardEvent) {
  const back = isColumn.value ? 'ArrowUp' : 'ArrowLeft'
  const forward = isColumn.value ? 'ArrowDown' : 'ArrowRight'
  let next: number | null = null
  if (event.key === back) next = percent.value - NUDGE
  else if (event.key === forward) next = percent.value + NUDGE
  else if (event.key === 'Home') next = MIN
  else if (event.key === 'End') next = MAX
  else if (event.key === 'Enter') next = 50
  if (next === null) return
  event.preventDefault()
  panels.setSplitPercent(next)
}
</script>

<template>
  <div
    class="dock-splitter"
    role="separator"
    tabindex="0"
    aria-label="Resize panels"
    :aria-orientation="isColumn ? 'horizontal' : 'vertical'"
    :aria-valuenow="Math.round(percent)"
    :aria-valuemin="MIN"
    :aria-valuemax="MAX"
    @pointerdown="onPointerDown"
    @keydown="onKeyDown"
    @dblclick="panels.setSplitPercent(50)"
  >
    <span class="grip" aria-hidden="true"></span>
  </div>
</template>

<style scoped>
.dock-splitter {
  grid-area: gutter;
  display: grid;
  place-items: center;
  background: var(--splitter-bg);
  /* Wider than it looks: the visible rule is the grip, the rest is grab room. */
  inline-size: 7px;
  cursor: col-resize;
  touch-action: none;
}

:global(.dock.split-column) > .dock-splitter {
  inline-size: auto;
  block-size: 7px;
  cursor: row-resize;
}

.grip {
  display: block;
  inline-size: 1px;
  block-size: 24px;
  background: var(--border);
  border-radius: var(--radius-pill);
}

:global(.dock.split-column) > .dock-splitter .grip {
  inline-size: 24px;
  block-size: 1px;
}

.dock-splitter:hover .grip,
.dock-splitter:focus-visible .grip {
  background: var(--brand);
  inline-size: 3px;
}

:global(.dock.split-column) > .dock-splitter:hover .grip {
  inline-size: 24px;
  block-size: 3px;
}
</style>
