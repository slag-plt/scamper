<script setup lang="ts">
import { nextTick, onMounted, ref, watch } from 'vue'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'
import CellEditor from './CellEditor.vue'
import type { ReplEntry } from '../composables/use-repl'
import type { CellEditorHandle } from '../codemirror/cell-editor'

/**
 * The REPL: a transcript of what has been tried, and a prompt to try the next
 * thing (#399).
 *
 * Reads as the file continued -- the entries are cells like the ones a notebook
 * will show -- but nothing here touches the file. What it is working from is
 * said once, in the banner, rather than repeated beside every entry.
 */
const props = defineProps<{
  entries: ReplEntry[]
  /** What the session was seeded from. */
  banner: string
  /** True while an entry is running: the prompt waits and Stop appears. */
  isBusy: boolean
  /** True once the file has been edited since this session was seeded. */
  isStale?: boolean
  /**
   * The program the prompt continues -- the file and the entries so far -- for
   * the language server to analyse it inside.
   */
  context?: string
}>()

const emit = defineEmits<{
  submit: [text: string]
  interrupt: []
  restart: []
}>()

/**
 * The document the prompt is, as far as the language server is concerned.
 *
 * One window, so one URI: it is opened when the window mounts and closed when
 * it goes, and a second window would need a second one.
 */
const PROMPT_URI = 'inmemory://repl-prompt.scm'

const scrollEl = ref<HTMLDivElement | null>(null)
const promptRef = ref<CellEditorHandle | null>(null)

/**
 * Where the caret is in the entry history: an index into `entries`, or null
 * when the prompt holds something typed rather than something recalled.
 */
const recalled = ref<number | null>(null)

function onSubmit(text: string) {
  if (props.isBusy) return
  promptRef.value?.clear()
  recalled.value = null
  emit('submit', text)
}

/**
 * Up and down at the edges of the prompt walk what has already been run, as
 * every terminal does. Entries with no source -- the one seeding reports into
 * -- are not entries anyone typed, so they are skipped.
 */
function onHistory(direction: -1 | 1, handled: { value: boolean }) {
  const typed = props.entries.filter((entry) => entry.source.trim().length > 0)
  if (typed.length === 0) return
  const at = recalled.value ?? typed.length
  const to = Math.min(typed.length, Math.max(0, at + direction))
  if (to === at) return
  handled.value = true
  recalled.value = to === typed.length ? null : to
  promptRef.value?.setText(to === typed.length ? '' : typed[to].source)
}

// Opened deliberately, from a button or a menu, so the caret belongs in the
// prompt: a REPL that has to be clicked into before it can be typed in is a
// REPL with an extra step in front of every use.
onMounted(() => {
  promptRef.value?.focus()
})

// A transcript is read from the bottom: a new entry, and whatever it prints,
// should be what is on screen when it arrives.
watch(
  () => [props.entries.length, props.isBusy] as const,
  () => {
    void nextTick(() => {
      if (scrollEl.value !== null) {
        scrollEl.value.scrollTop = scrollEl.value.scrollHeight
      }
    })
  },
)

defineExpose({
  focusPrompt: () => {
    promptRef.value?.focus()
  },
})
</script>

<template>
  <!-- Contents only: the window chrome is PanelFrame's business, as it is for
       the trace window. -->
  <div class="repl-contents">
    <!-- Pinned above the transcript rather than written into it, as the output
         pane's warning is: the transcript scrolls, and a notice that scrolls
         away is a notice nobody reads. -->
    <p v-if="isStale" class="repl-stale" role="status">
      <i class="fa-solid fa-triangle-exclamation" aria-hidden="true"></i>
      <em>
        Warning: the file has changed since this REPL started. Restart to pick
        up the changes.
      </em>
    </p>
    <!-- A log, so a screen reader announces what an entry produced rather than
         leaving it to be hunted for. -->
    <div ref="scrollEl" class="repl-scroll" role="log">
      <p class="repl-banner">{{ banner }}</p>

      <div v-for="entry in entries" :key="entry.id" class="repl-entry">
        <div v-if="entry.source.length > 0" class="repl-source">
          <span class="repl-marker" aria-hidden="true">&gt;</span>
          <CellEditor :source="entry.source" is-read-only />
        </div>
        <div
          v-for="(value, i) in entry.values"
          :key="i"
          class="repl-value"
        >
          <ValueRenderer :value="value" />
        </div>
      </div>

      <div class="repl-entry repl-prompt">
        <span class="repl-marker" aria-hidden="true">&gt;</span>
        <!-- The prompt is the only cell the language server holds: the entries
             above it are a record, and one document each would be a document
             per line ever typed. -->
        <CellEditor
          ref="promptRef"
          :lsp-uri="PROMPT_URI"
          :context="context"
          @submit="onSubmit"
          @history="onHistory"
        />
      </div>
    </div>

    <div class="repl-controls">
      <span class="repl-hint">
        Enter runs the line; Shift+Enter adds another.
      </span>
      <button
        v-if="isBusy"
        type="button"
        class="repl-button"
        title="Stop the entry that is running"
        @click="emit('interrupt')"
      >
        <i class="fa-solid fa-stop" aria-hidden="true"></i> Stop
      </button>
      <button
        type="button"
        class="repl-button"
        title="Start again from the file as it is now"
        @click="emit('restart')"
      >
        <i class="fa-solid fa-rotate-right" aria-hidden="true"></i> Restart
      </button>
    </div>
  </div>
</template>

<style scoped>
.repl-contents {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

/* The same strip the output pane uses for the same message, so the two read as
   one warning rather than two designs. */
.repl-stale {
  flex-shrink: 0;
  display: flex;
  align-items: baseline;
  gap: 0.4em;
  margin: 0;
  padding: 0.25em 0.5em;
  background: var(--surface-muted);
  border-bottom: 1px solid var(--border);
  font-size: 0.85em;
  color: var(--fg);
}

.repl-scroll {
  flex: 1;
  min-height: 0;
  overflow: auto;
  padding: 0.5em 0.6em;
}

.repl-banner {
  margin: 0 0 0.6em;
  font-size: 0.85em;
  font-style: italic;
  opacity: 0.75;
}

.repl-entry {
  margin-bottom: 0.35em;
}

/* The marker and the cell sit on one line, with the cell taking the rest of
   the width so a long entry wraps under itself rather than under the marker. */
.repl-source,
.repl-prompt {
  display: flex;
  align-items: flex-start;
  gap: 0.4em;
}

.repl-source > :deep(*),
.repl-prompt > :deep(*) {
  flex: 1;
  min-width: 0;
}

.repl-marker {
  flex: 0 0 auto;
  opacity: 0.5;
  font-family: var(--font-mono, monospace);
  user-select: none;
}

/* Output is indented under the entry that produced it, so the eye can follow
   the markers down the transcript. */
.repl-value {
  padding: 0.15em 0 0.15em 1.2em;
  white-space: pre-wrap;
}

.repl-controls {
  flex-shrink: 0;
  display: flex;
  align-items: center;
  gap: 0.5em;
  border-top: 1px solid var(--border);
  background: var(--surface-muted);
  padding: 0.35em 0.5em;
}

.repl-hint {
  flex: 1;
  min-width: 0;
  font-size: 0.75em;
  opacity: 0.7;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.repl-button {
  flex-shrink: 0;
  border: none;
  background: none;
  padding: 0.3em 0.5em;
  font-size: 0.85em;
  color: inherit;
  border-radius: 4px;
  cursor: pointer;
}

.repl-button:hover {
  background: color-mix(in srgb, currentColor 20%, transparent);
}
</style>
