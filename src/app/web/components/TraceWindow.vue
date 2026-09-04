<script setup lang="ts">
import { computed, provide, watch } from 'vue'
import type { Value } from '../../../lpm'
import * as U from '../../../lpm/util'
import { expToLayout, isExp, type Layout } from '../../../scheme/ast'
import { changedLayoutPath } from '../../../scheme/layout-diff'
import { ChangedPathKey } from '../../../scheme/ast-components/changed-path'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'
import SourceCaption from './SourceCaption.vue'

/**
 * A statement's reduction trace, one step at a time.
 *
 * The output pane shows a whole trace at once, which is the wrong shape for
 * following a single statement: the interesting thing is the step you are on,
 * not the fifty around it. So one step fills the window and the controls at
 * the foot move between them -- ends, neighbours, or anywhere via the slider,
 * which is the only control that makes a long trace navigable at all.
 */
const props = defineProps<{
  /** The statement being traced, shown as a heading. */
  source: string
  steps: Value[]
  /** True when the trace hit its step limit and the rest was dropped. */
  truncated?: boolean
}>()

/** Which step is showing, zero-based. */
const index = defineModel<number>('index', { default: 0 })

const count = computed(() => props.steps.length)
const atStart = computed(() => index.value <= 0)
const atEnd = computed(() => index.value >= count.value - 1)

/** Keeps the position inside the trace when a new one replaces this one. */
watch(count, () => {
  index.value = 0
})

function go(to: number) {
  index.value = Math.max(0, Math.min(to, count.value - 1))
}

/**
 * The layout a step draws, unwrapped from the trace value carrying it.
 * @returns null for a step that is not an expression -- an error, say -- which
 *          nothing can meaningfully be diffed against.
 */
function stepLayout(step: Value | undefined): Layout | null {
  if (step === undefined) return null
  const inner =
    U.isStructKind(step, 'trace-output') || U.isStructKind(step, 'trace-start')
      ? (step as { output?: Value }).output
      : step
  return isExp(inner) ? expToLayout(inner) : null
}

/**
 * Where the shown step differs from the one before it, for the renderer to
 * highlight. Null on the first step, and whenever either side is not an
 * expression: with nothing to compare, highlighting everything would say less
 * than highlighting nothing.
 */
const changedPath = computed(() => {
  const before = stepLayout(props.steps[index.value - 1])
  const after = stepLayout(props.steps[index.value])
  return before === null || after === null
    ? null
    : changedLayoutPath(before, after)
})

provide(ChangedPathKey, changedPath)

/** The slider hands back a string; it is the only control that needs parsing. */
function onSeek(event: Event) {
  go(Number((event.target as HTMLInputElement).value))
}
</script>

<template>
  <!-- Contents only: the window chrome, and whether there is any, is
       PanelFrame's business now. -->
  <div class="trace-contents">
    <div class="trace-body">
      <SourceCaption class="trace-source" :source="source" force-visible />
      <div class="trace-step">
        <!-- Nothing kept *and* cut short means the limit was spent before this
             statement was reached: an earlier one runs forever, so this one
             never ran at all (#369). Saying it takes no steps would blame the
             wrong statement. -->
        <p v-if="count === 0 && truncated" class="trace-empty">
          Stopped: a statement before this one runs longer than the trace step
          limit.
        </p>
        <p v-else-if="count === 0" class="trace-empty">
          This statement takes no visible steps.
        </p>
        <ValueRenderer v-else :value="steps[index]" />
      </div>
      <p v-if="truncated && count > 0" class="trace-truncated" role="status">
        Stopped after {{ count }} steps — this statement has more than can be
        traced.
      </p>
    </div>

    <div class="trace-controls">
      <div class="trace-buttons">
        <button
          type="button"
          class="fa-solid fa-angles-left"
          title="First step"
          aria-label="First step"
          :disabled="atStart"
          @click="go(0)"
        ></button>
        <button
          type="button"
          class="fa-solid fa-angle-left"
          title="Previous step"
          aria-label="Previous step"
          :disabled="atStart"
          @click="go(index - 1)"
        ></button>

        <div class="trace-seek">
          <input
            class="trace-slider"
            type="range"
            min="0"
            :max="Math.max(0, count - 1)"
            :value="index"
            :disabled="count <= 1"
            aria-label="Step"
            @input="onSeek"
          />
          <span class="trace-count" aria-live="polite">
            {{ count === 0 ? 0 : index + 1 }}/{{ count }}
          </span>
        </div>

        <button
          type="button"
          class="fa-solid fa-angle-right"
          title="Next step"
          aria-label="Next step"
          :disabled="atEnd"
          @click="go(index + 1)"
        ></button>
        <button
          type="button"
          class="fa-solid fa-angles-right"
          title="Last step"
          aria-label="Last step"
          :disabled="atEnd"
          @click="go(count - 1)"
        ></button>
      </div>
    </div>
  </div>
</template>

<style scoped>
.trace-contents {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.trace-body {
  flex: 1;
  min-height: 0;
  overflow: auto;
  padding: 0.5em 0.6em;
}

/* The statement being stepped, pinned above its steps. Shown always, unlike
   the captions in the output pane -- here it is the window's subject. */
.trace-source {
  margin-top: 0;
}

.trace-step {
  padding: 0.6em 0.2em;
  white-space: pre-wrap;
}

.trace-empty,
.trace-truncated {
  margin: 0.4em 0 0;
  font-size: 0.85em;
  font-style: italic;
  opacity: 0.75;
}

/* The controls sit at the foot of the window, out of the trace's way. */
.trace-controls {
  flex-shrink: 0;
  border-top: 1px solid var(--border);
  background: var(--surface-muted);
  padding: 0.35em 0.5em;
}

.trace-buttons {
  display: flex;
  align-items: center;
  gap: 0.35em;
}

.trace-buttons button {
  flex-shrink: 0;
  border: none;
  background: none;
  padding: 0.3em 0.5em;
  font-size: 0.9em;
  color: inherit;
  border-radius: 4px;
  cursor: pointer;
}

.trace-buttons button:hover:not(:disabled) {
  background: color-mix(in srgb, currentColor 20%, transparent);
}

.trace-buttons button:disabled {
  opacity: 0.35;
  cursor: default;
}

/* The slider takes the middle, with its position reading underneath -- so the
   count sits under the thumb's travel rather than off at one end. */
.trace-seek {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.1em;
}

.trace-slider {
  width: 100%;
  margin: 0;
  accent-color: var(--accent);
  cursor: pointer;
}

.trace-slider:disabled {
  opacity: 0.4;
  cursor: default;
}

.trace-count {
  font-size: 0.75em;
  font-variant-numeric: tabular-nums;
  opacity: 0.75;
}
</style>
