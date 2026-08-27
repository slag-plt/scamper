<script setup lang="ts">
import { computed } from 'vue'
import { usePanels } from '../composables/use-panels'
import type { PanelId, SlotId } from '../panel-layout'

/**
 * One dock slot's tab strip.
 *
 * Drawn only when the slot has something to label -- see PanelDock. The tabs
 * are in canonical panel order and are not reorderable: with three panels the
 * value of dragging them about is close to zero, and it is the only thing that
 * would make the model need an explicit tab order rather than deriving one.
 */
const props = defineProps<{
  /** Named slotId, not slot: `slot` is a reserved attribute name in Vue. */
  slotId: SlotId
  /** Display names and close affordances, keyed by panel. */
  labels: Record<PanelId, string>
  closeable?: readonly PanelId[]
}>()

const emit = defineEmits<{ close: [id: PanelId] }>()

const panels = usePanels()

const tabs = computed(() => panels.tabs(props.slotId))
/** False where the layout is a forced projection rather than the real thing. */
const canPlace = computed(() => !panels.isProjected.value)
const active = computed(() => panels.active(props.slotId))

function isCloseable(id: PanelId): boolean {
  return props.closeable?.includes(id) ?? false
}

/*
 * Both take the active panel's id first.
 *
 * Reading `active` twice in one template expression does not work: the command
 * changes which panel is active, so the second read sees the new answer -- or
 * null, once the panel has left the slot entirely.
 */
function floatActive() {
  const id = active.value
  if (id === null) return
  panels.float(id)
  panels.focusPanel(id)
}

function moveActive() {
  const id = active.value
  if (id === null) return
  panels.moveToOtherSlot(id)
  panels.focusPanel(id)
}

/** Left/Right move between tabs, as a tablist is expected to. */
function onKey(event: KeyboardEvent) {
  const order = tabs.value
  if (order.length === 0) return
  const at = order.indexOf(active.value ?? order[0])
  let to = at
  if (event.key === 'ArrowRight') to = (at + 1) % order.length
  else if (event.key === 'ArrowLeft') to = (at - 1 + order.length) % order.length
  else if (event.key === 'Home') to = 0
  else if (event.key === 'End') to = order.length - 1
  else return
  event.preventDefault()
  panels.reveal(order[to])
}
</script>

<template>
  <div class="pane-tabs" role="tablist" @keydown="onKey">
    <!-- A closeable panel's tab carries its own close button. Docked, the
         panel has no title bar to put one in, so without this a trace opened
         on a narrow pane could never be dismissed. The wrapper is
         presentational so the tablist still sees only tabs. -->
    <div
      v-for="id in tabs"
      :key="id"
      role="presentation"
      class="pane-tab-slot"
      :class="{ selected: active === id }"
    >
      <button
        :id="`tab-${id}`"
        type="button"
        role="tab"
        class="pane-tab"
        :aria-selected="active === id"
        :aria-controls="`panel-${id}`"
        :tabindex="active === id ? 0 : -1"
        @click="panels.reveal(id)"
      >
        {{ labels[id] }}
      </button>
      <button
        v-if="isCloseable(id)"
        type="button"
        class="pane-tab-close fa-solid fa-xmark"
        :title="`Close ${labels[id]}`"
        :aria-label="`Close ${labels[id]}`"
        @click="emit('close', id)"
      ></button>
    </div>
    <!-- Slot controls, acting on whichever tab is showing. They live here
         rather than on each tab because they are about where the panel goes,
         and only one panel can be looked at at a time anyway.

         Hidden on a narrow pane: the compact layout is a projection that tabs
         everything regardless, so these would edit the stored arrangement
         while the screen stayed put -- a button that reads as broken now and
         surprises you when the pane widens. IdeApp's View-menu equivalent goes
         empty there for the same reason. -->
    <div v-if="active !== null && canPlace" class="slot-controls">
      <button
        type="button"
        class="slot-button fa-solid fa-right-left"
        :title="`Move ${labels[active]} to the other side`"
        :aria-label="`Move ${labels[active]} to the other side`"
        @click="moveActive"
      ></button>
      <button
        type="button"
        class="slot-button fa-solid fa-window-restore"
        :title="`Float ${labels[active]}`"
        :aria-label="`Float ${labels[active]}`"
        @click="floatActive"
      ></button>
    </div>
  </div>
</template>

<style scoped>
.pane-tabs {
  display: flex;
  flex-shrink: 0;
  background: var(--header-bg);
  border-bottom: 1px solid var(--border);
}

.pane-tab-slot {
  flex: 1;
  display: flex;
  align-items: stretch;
  border-bottom: 2px solid transparent;
  min-width: 0;
}

.pane-tab-slot.selected {
  border-bottom-color: var(--brand);
  background: var(--surface);
}

.pane-tab {
  flex: 1;
  min-width: 0;
  border: none;
  background: none;
  padding: var(--space-sm) var(--space-lg);
  font: inherit;
  font-size: var(--text-md);
  color: inherit;
  cursor: pointer;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.pane-tab-close {
  flex-shrink: 0;
  border: none;
  background: none;
  padding: 0 var(--space-md);
  font-size: var(--text-xs);
  color: inherit;
  cursor: pointer;
}

.pane-tab-close:hover {
  background: color-mix(in srgb, currentColor 18%, transparent);
}

.slot-controls {
  flex-shrink: 0;
  display: flex;
  align-items: center;
  gap: var(--space-2xs);
  padding-inline: var(--space-xs);
}

.slot-button {
  border: none;
  background: none;
  padding: var(--space-2xs) var(--space-xs);
  font-size: var(--text-xs);
  color: inherit;
  opacity: 0.7;
  cursor: pointer;
  border-radius: var(--radius-sm);
}

.slot-button:hover {
  opacity: 1;
  background: var(--surface-hover);
}
</style>
