<script setup lang="ts">
import { computed } from 'vue'
import DockSplitter from './DockSplitter.vue'
import PanelTabs from './PanelTabs.vue'
import { usePanels } from '../composables/use-panels'
import type { PanelId, SlotId } from '../panel-layout'

/**
 * The box the panels live in.
 *
 * A CSS grid with a tab-strip row over a body row per slot, and `position:
 * relative` so floating panels can be absolutely positioned children of the
 * same element. Docked panels are grid items; floating ones are overlays. That
 * is what lets a panel move between the two without moving in the component
 * tree -- see PanelFrame for why that matters.
 *
 * The frames themselves are passed in as a slot, so IdeApp keeps ownership of
 * what goes inside each one.
 */
const props = defineProps<{
  labels: Record<PanelId, string>
  closeable?: readonly PanelId[]
}>()

const emit = defineEmits<{ close: [id: PanelId] }>()

const panels = usePanels()

const slots = computed(() => panels.slots.value)

/**
 * Whether `slot` draws its tab strip.
 *
 * One sentence: a slot shows its tabs only when it holds more than one, since
 * a strip exists to choose between them.
 *
 * A lone panel is therefore bare in either slot, which is what keeps the
 * default arrangement -- code and output side by side (#371) -- looking as it
 * did before the dock existed, with no chrome over the code. Float and Dock
 * for a bare panel live in the View menu, which is the surface that can always
 * reach every panel.
 */
function hasStrip(slot: SlotId): boolean {
  return panels.tabs(slot).length > 1
}

const isSplit = computed(() => slots.value.length > 1)
</script>

<template>
  <div
    class="dock"
    :class="isSplit ? `split-${panels.effective.value.axis}` : 'single'"
    :style="{ '--split-a': `${String(panels.effective.value.splitPercent)}%` }"
  >
    <PanelTabs
      v-if="hasStrip('a')"
      class="tabs-a"
      slot-id="a"
      :labels="props.labels"
      :closeable="props.closeable"
      @close="emit('close', $event)"
    />
    <PanelTabs
      v-if="hasStrip('b')"
      class="tabs-b"
      slot-id="b"
      :labels="props.labels"
      :closeable="props.closeable"
      @close="emit('close', $event)"
    />
    <DockSplitter v-if="isSplit" />
    <slot></slot>
  </div>
</template>

<style scoped>
.dock {
  position: relative;
  overflow: hidden;
  display: grid;
  /*
   * `minmax(0, ...)` on every track is required rather than tidy: without it a
   * track takes its content's intrinsic size as a floor, and CodeMirror's is
   * as wide as its longest line -- which blows the column out and gives the
   * whole pane a horizontal scrollbar.
   */
  grid-template-columns: minmax(0, 1fr);
  /* The strip row is `auto`, so it collapses to nothing when absent. */
  grid-template-rows: auto minmax(0, 1fr);
  grid-template-areas:
    'tabs-a'
    'body-a';
}

.dock.split-row {
  grid-template-columns: minmax(0, var(--split-a)) auto minmax(0, 1fr);
  grid-template-areas:
    'tabs-a gutter tabs-b'
    'body-a gutter body-b';
}

.dock.split-column {
  grid-template-rows: auto minmax(0, var(--split-a)) auto auto minmax(0, 1fr);
  grid-template-areas:
    'tabs-a'
    'body-a'
    'gutter'
    'tabs-b'
    'body-b';
}

.tabs-a {
  grid-area: tabs-a;
}

.tabs-b {
  grid-area: tabs-b;
}
</style>
