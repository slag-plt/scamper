<script setup lang="ts">
import { computed } from 'vue'

/**
 * An error, laid out rather than flattened.
 *
 * `ScamperError.toString()` produces one line that opens with machinery --
 * `Runtime error [7:1-7:12]: (error) ...` -- so the first thing a student reads
 * is a phase name and a coordinate range, and the sentence telling them what is
 * actually wrong comes last. Here the message leads and the rest becomes a
 * muted second line.
 *
 * The parts are read structurally rather than by importing ScamperError: this
 * is a presentational component, and pulling the LPM's error module into the
 * renderer graph gave the module loader a cycle to trip over. Anything without
 * those fields -- a plain Error escaping the runtime -- still renders, just
 * without the second line.
 */
interface ErrorParts {
  phase?: unknown
  modName?: unknown
  source?: unknown
  range?: { begin?: { line?: unknown; col?: unknown } }
}

const props = defineProps<{ value: Error }>()

const parts = computed<ErrorParts>(() => props.value as Error & ErrorParts)

const str = (v: unknown): string | null =>
  typeof v === 'string' && v.length > 0 ? v : null

/** The problem itself, which is what the reader needs first. */
const message = computed(() => props.value.message)

/**
 * Where it happened, in words rather than in `[line:col-line:col]`.
 *
 * Only the start: the end of the range is machinery, and the beginning is the
 * only part of it anyone navigates to. A line of -1 is the LPM's "no location".
 */
const where = computed(() => {
  const begin = parts.value.range?.begin
  const line = begin?.line
  const col = begin?.col
  if (typeof line !== 'number' || line < 0) return null
  const at = `line ${String(line)}`
  return typeof col === 'number' && col >= 0
    ? `${at}, column ${String(col)}`
    : at
})

/**
 * The phase and the location: what kind of error, and where.
 *
 * Deliberately not `source`, the built-in that raised it. toString() puts it in
 * front of the message as `(error) ...`, and on a line of its own it reads as a
 * second, contentless word -- "Runtime error · error" for anything raised
 * through `error`. The message already says what went wrong.
 */
const origin = computed(() => {
  const phase = str(parts.value.phase)
  const bits = [
    phase === null ? null : `${phase} error`,
    str(parts.value.modName),
    where.value,
  ].filter((b): b is string => b !== null)
  return bits.length > 0 ? bits.join(' · ') : null
})
</script>

<template>
  <div class="error" tabindex="0">
    <span class="error-mark" aria-hidden="true">&#9888;</span>
    <div class="error-body">
      <strong class="error-message">{{ message }}</strong>
      <span v-if="origin !== null" class="error-origin">{{ origin }}</span>
    </div>
  </div>
</template>

<style scoped>
/*
 * --danger is the palette's one red, and it is tuned to clear AA as text in
 * both themes. (There used to be a second, --error-accent, whose light value
 * was a pale background tint reading ~1.8:1 as text; it is gone.)
 */
.error {
  display: flex;
  align-items: flex-start;
  gap: var(--space-md);
  padding: var(--space-md) var(--space-lg);
  background: var(--test-error-bg);
  border-left: 3px solid var(--danger);
  border-radius: var(--radius-sm);
  color: var(--fg);
}

.error-mark {
  flex-shrink: 0;
  color: var(--danger);
  font-size: var(--text-base);
  line-height: var(--leading-tight);
}

.error-body {
  display: flex;
  flex-direction: column;
  gap: var(--space-2xs);
  min-width: 0;
}

.error-message {
  font-weight: 600;
  line-height: var(--leading-tight);
  /* Errors are prose, so let them wrap like prose even inside the output
     pane's pre-wrap. */
  white-space: normal;
}

.error-origin {
  font-family: var(--font-mono);
  font-size: var(--text-xs);
  color: var(--fg);
  opacity: 0.7;
  white-space: normal;
}
</style>
