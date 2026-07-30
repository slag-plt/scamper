<script setup lang="ts">
import { ref } from 'vue'
import { useOutputPane } from '../composables/use-output-pane'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'

const scrollEl = ref<HTMLDivElement | null>(null)
const { reset, blocks, virtualizer, display, scrollToBottom } =
  useOutputPane(scrollEl)

defineExpose({ display, reset, scrollToBottom })
</script>

<template>
  <div ref="scrollEl" class="output-scroll">
    <div
      :style="{
        position: 'relative',
        height: virtualizer.getTotalSize() + 'px',
        width: '100%',
      }"
    >
      <div
        v-for="row in virtualizer.getVirtualItems()"
        :key="row.index"
        :ref="(el) => virtualizer.measureElement(el as Element | null)"
        :data-index="row.index"
        :class="blocks[row.index].attrs"
        :style="{
          position: 'absolute',
          top: 0,
          left: 0,
          width: '100%',
          boxSizing: 'border-box',
          transform: `translateY(${row.start}px)`,
        }"
      >
        <ValueRenderer
          v-if="'value' in blocks[row.index]"
          :value="blocks[row.index].value"
        />
      </div>
    </div>
  </div>
</template>

<style scoped>
.output-scroll {
  overflow: auto;
  height: 100%;
  white-space: pre-wrap;
  content-visibility: auto;
}

/* Outer Trace Block */
.trace-top {
  height: 0.75em;
  position: relative;
}
.trace-top::after {
  content: "";
  position: absolute;
  top: 0.25em;
  bottom: 0;
  left: 0;
  right: 0;
  background-color: var(--surface-muted);
  border: 0.1em solid var(--border);
  border-bottom: none;
  border-top-left-radius: 0.5em;
  border-top-right-radius: 0.5em;
  z-index: -1;
}

.trace-item {
  position: relative;
  padding: 0.25em 0.5em;
}
.trace-item::after {
  content: "";
  position: absolute;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  background-color: var(--surface-muted);
  border-left: 0.1em solid var(--border);
  border-right: 0.1em solid var(--border);
  z-index: -1;
}

.trace-bottom {
  height: 0.75em;
  position: relative;
}
.trace-bottom::after {
  content: "";
  position: absolute;
  top: 0;
  bottom: 0.25em;
  left: 0;
  right: 0;
  background-color: var(--surface-muted);
  border: 0.1em solid var(--border);
  border-top: none;
  border-bottom-left-radius: 0.5em;
  border-bottom-right-radius: 0.5em;
  z-index: -1;
}

/* Inner Trace Steps Block */
.trace-steps-top {
  height: 0.5em;
  position: relative;
}
.trace-steps-top::before {
  content: "";
  position: absolute;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  background-color: var(--surface-muted);
  border-left: 0.1em solid var(--border);
  border-right: 0.1em solid var(--border);
  z-index: -2;
}
.trace-steps-top::after {
  content: "";
  position: absolute;
  top: 0.25em;
  bottom: 0;
  left: 1em;
  right: 1em;
  background-color: var(--surface);
  border: 0.1em dashed var(--border);
  border-bottom: none;
  border-top-left-radius: 0.5em;
  border-top-right-radius: 0.5em;
  z-index: -1;
}

.trace-steps-item {
  position: relative;
  padding: 0.25em 1.5em;
}
.trace-steps-item::before {
  content: "";
  position: absolute;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  background-color: var(--surface-muted);
  border-left: 0.1em solid var(--border);
  border-right: 0.1em solid var(--border);
  z-index: -2;
}
.trace-steps-item::after {
  content: "";
  position: absolute;
  top: 0;
  bottom: 0;
  left: 1em;
  right: 1em;
  background-color: var(--surface);
  border-left: 0.1em dashed var(--border);
  border-right: 0.1em dashed var(--border);
  z-index: -1;
}

.trace-steps-bottom {
  height: 0.5em;
  position: relative;
}
.trace-steps-bottom::before {
  content: "";
  position: absolute;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  background-color: var(--surface-muted);
  border-left: 0.1em solid var(--border);
  border-right: 0.1em solid var(--border);
  z-index: -2;
}
.trace-steps-bottom::after {
  content: "";
  position: absolute;
  top: 0;
  bottom: 0.25em;
  left: 1em;
  right: 1em;
  background-color: var(--surface);
  border: 0.1em dashed var(--border);
  border-top: none;
  border-bottom-left-radius: 0.5em;
  border-bottom-right-radius: 0.5em;
  z-index: -1;
}

.base-item {
  padding: 0.25em;
}
</style>
