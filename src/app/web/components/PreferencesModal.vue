<script setup lang="ts">
import { useId } from 'vue'
import AppModal from './AppModal.vue'
import {
  MAX_CALL_STACK_DEPTH,
  MAX_TRACE_STEP_LIMIT,
  MIN_CALL_STACK_DEPTH,
  MIN_TRACE_STEP_LIMIT,
  checkExamples,
  liveEvaluation,
  maxCallStackDepth,
  setCheckExamples,
  setLiveEvaluation,
  setMaxCallStackDepth,
  setTraceStepLimit,
  traceStepLimit,
} from '../run-prefs'
import {
  MAX_FONT_SIZE,
  MIN_FONT_SIZE,
  autoSuggest,
  editorFontSize,
  editorWordWrap,
  isRelaxedFormatting,
  setAutoSuggest,
  setEditorFontSize,
  setEditorWordWrap,
  setFormatMode,
} from '../editor-prefs'
import {
  setShowSourceWithOutput,
  showSourceWithOutput,
} from '../output-prefs'
import { setShowHiddenFiles, showHiddenFiles } from '../file-prefs'
import { fileView, setFileView } from '../view-prefs'
import { currentTheme, setTheme } from '../../../theme'

/**
 * One place for every setting Scamper remembers (issue #497).
 *
 * The menus keep the toggles worth a single click; this is where all of them
 * can be found at once, and the only home for the two that are numbers -- the
 * trace step limit and the recursion depth -- since a menu row that exists to
 * display a number is what prompted the pane in the first place.
 *
 * What is *not* here is what is not remembered: which panels are floating,
 * whether the file drawer is open. Those are the shape of the window right now
 * rather than a choice carried into tomorrow, and they belong in View.
 *
 * It takes no preferences as props. Every one is a module-level ref that
 * persists itself (run-prefs, editor-prefs, output-prefs, file-prefs,
 * view-prefs, theme), read here exactly as IdeMenuBar reads them, so the pane
 * and the menus cannot disagree about what is set.
 */
defineProps<{ open: boolean }>()
const emit = defineEmits<{ close: [] }>()

/** Reads long limits the way a person writes them: 100000 -> "100,000". */
function commas(n: number): string {
  // Pinned rather than the ambient locale: this is English prose either way,
  // and the tests should not depend on where they run.
  return n.toLocaleString('en-US')
}

interface RowBase {
  label: string
  /** What the setting costs, or what it means; shown under the control. */
  note?: string
}

interface ToggleRow extends RowBase {
  kind: 'toggle'
  get: () => boolean
  set: (on: boolean) => void
}

interface NumberRow extends RowBase {
  kind: 'number'
  note: string
  min: number
  max: number
  get: () => number
  set: (value: number) => void
}

type Row = ToggleRow | NumberRow

interface Section {
  title: string
  rows: Row[]
}

/**
 * The pane, as data -- the shape the menus are already built in (see menu.ts).
 *
 * `get` is called during render, so reading a ref there is what subscribes the
 * pane to it; the list itself is a plain constant and never has to be rebuilt.
 */
const sections: Section[] = [
  {
    title: 'Execution',
    rows: [
      {
        kind: 'toggle',
        label: 'Live evaluation',
        note: 'Runs the file automatically.',
        get: () => liveEvaluation.value,
        set: setLiveEvaluation,
      },
      {
        kind: 'toggle',
        label: 'Check examples',
        note: 'Automatically executes @example tags found in docstrings.',
        get: () => checkExamples.value,
        set: setCheckExamples,
      },
      {
        kind: 'number',
        label: 'Trace step limit',
        note: 'The maximum number of steps that the stepper takes.',
        min: MIN_TRACE_STEP_LIMIT,
        max: MAX_TRACE_STEP_LIMIT,
        get: () => traceStepLimit.value,
        set: setTraceStepLimit,
      },
      {
        kind: 'number',
        label: 'Maximum stack depth',
        note: 'The maximum number of active function calls during execution.',
        min: MIN_CALL_STACK_DEPTH,
        max: MAX_CALL_STACK_DEPTH,
        get: () => maxCallStackDepth.value,
        set: setMaxCallStackDepth,
      },
    ],
  },
  {
    title: 'Editing',
    rows: [
      {
        kind: 'number',
        label: 'Font size',
        note: `The editor's text, in pixels, between ${commas(MIN_FONT_SIZE)} and ${commas(MAX_FONT_SIZE)}.`,
        min: MIN_FONT_SIZE,
        max: MAX_FONT_SIZE,
        get: () => editorFontSize.value,
        set: setEditorFontSize,
      },
      {
        kind: 'toggle',
        label: 'Word wrap',
        get: () => editorWordWrap.value,
        set: setEditorWordWrap,
      },
      {
        kind: 'toggle',
        label: 'Relaxed formatting',
        get: isRelaxedFormatting,
        set: (on) => {
          setFormatMode(on ? 'relaxed' : 'strict')
        },
      },
      {
        kind: 'toggle',
        label: 'Suggest as you type',
        note: 'Automatically offer auto-completion suggestions and parameter help.',
        get: () => autoSuggest.value,
        set: setAutoSuggest,
      },
    ],
  },
  {
    title: 'Display',
    rows: [
      {
        kind: 'toggle',
        label: 'Show programs as a notebook',
        note: 'Enable notebook view where Scamper source code and output is interleaved together.',
        get: () => fileView.value === 'notebook',
        set: (on) => {
          setFileView(on ? 'notebook' : 'source')
        },
      },
      {
        kind: 'toggle',
        label: 'Source with output',
        note: 'Prepends each outputted value with the source code that generated it.',
        get: () => showSourceWithOutput.value,
        set: setShowSourceWithOutput,
      },
      {
        kind: 'toggle',
        label: 'Show hidden files',
        note: "Lists hidden system files (i.e., starts with '.')",
        get: () => showHiddenFiles.value,
        set: setShowHiddenFiles,
      },
      {
        kind: 'toggle',
        label: 'Dark theme',
        get: () => currentTheme.value === 'dark',
        set: (on) => {
          setTheme(on ? 'dark' : 'light')
        },
      },
    ],
  },
]

// One base id, suffixed per row and per heading. A note is tied to its control
// by aria-describedby, so a screen reader reaches the cost along with the
// setting, and each section names itself, so "Trace step limit" is heard as
// part of Running rather than as one of twelve loose controls.
const idBase = useId()

function noteId(section: number, row: number): string {
  return `${idBase}-${String(section)}-${String(row)}`
}

function headingId(section: number): string {
  return `${idBase}-${String(section)}`
}

function onToggle(row: ToggleRow, event: Event): void {
  row.set((event.target as HTMLInputElement).checked)
}

/**
 * Applies a number the user has finished typing, on change rather than on
 * input: clamping every keystroke would rewrite `20` on the way to `2000`.
 *
 * A blank or unreadable box is put back rather than clamped up to the floor,
 * which is the distinction the Run menu's prompt drew -- a deliberate `5` means
 * the floor, an empty box means nothing. The box is rewritten either way, since
 * a clamp that leaves the typed number on screen is a clamp nobody sees.
 */
function onNumber(row: NumberRow, event: Event): void {
  const input = event.target as HTMLInputElement
  const value = Number(input.value.trim())
  if (input.value.trim() !== '' && Number.isFinite(value)) {
    row.set(value)
  }
  input.value = String(row.get())
}
</script>

<template>
  <AppModal :open="open" title="Preferences" @dismiss="emit('close')">
    <div class="prefs">
      <section
        v-for="(section, s) in sections"
        :key="section.title"
        class="prefs__section"
        role="group"
        :aria-labelledby="headingId(s)"
      >
        <h3 :id="headingId(s)" class="prefs__heading">{{ section.title }}</h3>
        <div v-for="(row, r) in section.rows" :key="row.label" class="prefs__row">
          <label v-if="row.kind === 'toggle'" class="prefs__toggle">
            <input
              type="checkbox"
              :checked="row.get()"
              :aria-describedby="row.note ? noteId(s, r) : undefined"
              @change="onToggle(row, $event)"
            />
            <span>{{ row.label }}</span>
          </label>
          <label v-else class="prefs__number">
            <span>{{ row.label }}</span>
            <input
              type="number"
              :min="row.min"
              :max="row.max"
              :value="row.get()"
              :aria-describedby="noteId(s, r)"
              @change="onNumber(row, $event)"
            />
          </label>
          <p v-if="row.note" :id="noteId(s, r)" class="prefs__note">
            {{ row.note }}
          </p>
        </div>
      </section>
    </div>
    <template #footer>
      <button type="button" class="prefs__button" autofocus @click="emit('close')">
        Done
      </button>
    </template>
  </AppModal>
</template>

<style scoped>
.prefs {
  display: flex;
  flex-direction: column;
  gap: var(--space-lg);
  /* Twelve settings is more than fits a short window; the footer stays put. */
  max-height: 60vh;
  overflow-y: auto;
}

.prefs__section {
  display: flex;
  flex-direction: column;
  gap: var(--space-md);
}

.prefs__heading {
  margin: 0;
  padding-bottom: var(--space-2xs);
  border-bottom: 1px solid var(--border-muted);
  font-size: var(--text-md);
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  opacity: 0.7;
}

.prefs__row {
  display: flex;
  flex-direction: column;
  gap: var(--space-2xs);
}

.prefs__toggle {
  display: flex;
  align-items: center;
  gap: var(--space-sm);
  cursor: pointer;
}

.prefs__number {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--space-sm);
}

.prefs__number input {
  width: 7rem;
  padding: 0.25rem 0.4rem;
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  background-color: var(--surface);
  color: var(--fg);
  font: inherit;
  font-variant-numeric: tabular-nums;
}

.prefs__toggle input:focus-visible,
.prefs__number input:focus-visible {
  outline: 2px solid var(--focus);
  outline-offset: 1px;
}

.prefs__note {
  margin: 0;
  font-size: var(--text-xs);
  line-height: var(--leading-normal);
  opacity: 0.6;
}

.prefs__button {
  padding: 0.4rem 0.9rem;
  border: 1px solid var(--accent);
  border-radius: var(--radius-md);
  background-color: var(--accent);
  color: var(--accent-fg);
  font: inherit;
  cursor: pointer;
}

.prefs__button:hover {
  filter: brightness(1.05);
}
</style>
