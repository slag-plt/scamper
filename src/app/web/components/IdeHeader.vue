<script setup lang="ts">
import { computed, ref } from 'vue'
import { useScamperSession } from '../composables/use-scamper-session'
import type { MenuItem } from '../menu'
import type { LiveStatus } from '../composables/use-live-evaluation'
import { toggleLiveEvaluation } from '../run-prefs'
import { toggleFileView } from '../view-prefs'
import { appShortcut } from '../edit-commands'
import ThemeToggle from '../../shared/ThemeToggle.vue'
import PopupMenu from './PopupMenu.vue'
import ShortcutsHelp from './ShortcutsHelp.vue'

const props = withDefaults(
  defineProps<{
    /** False when the cursor is not inside a statement, so there is none to step. */
    canStep?: boolean
    /** True while a trace is being collected, which takes a moment. */
    isStepping?: boolean
    /** What live evaluation is doing, which the Run control shows (#378). */
    liveStatus?: LiveStatus
    /**
     * False when the open file is not a Scamper program, so there is nothing
     * to run (#385): a text file is not a program, and a binary one never
     * reaches the editor at all.
     */
    canRun?: boolean
    /** Whether the file is being shown as a notebook rather than as source. */
    isNotebook?: boolean
  }>(),
  // `canRun` defaults true rather than being left absent: Vue casts a missing
  // boolean prop to false, which would disable Run for every caller that does
  // not pass it.
  { liveStatus: 'off', canRun: true },
)

const emit = defineEmits<{
  toggleSidebar: []
  stepStatement: []
  openRepl: []
}>()

const session = useScamperSession()

const isRunInProgress = computed(() => session.currentRun.value !== null)

async function handleRun() {
  await session.execute()
}

// ---------- the Run control (#378) ----------

/**
 * The Run button is a split control: pressing the left half runs the file, and
 * the right half opens the run menu and is where live evaluation shows itself.
 *
 * The word on it is "Autorun" whenever live evaluation is on and "Run"
 * otherwise, and the control animates while a run is coming or going -- which
 * is the only sign a student gets that the output they are watching is about
 * to be replaced.
 */

/** True when live evaluation is on, whatever it happens to be doing. */
const isLive = computed(() => props.liveStatus !== 'off')

/**
 * The word on the pill: the action it performs, or -- while live evaluation is
 * on -- the fact that it is being performed without being asked.
 */
const runLabel = computed(() => (isLive.value ? 'Autorun' : 'Run'))

/**
 * True when the run in flight is live evaluation's own.
 *
 * Only `running` counts. `pending` says a run is *coming*, which an edit sets
 * even while a manual run is still going -- and that run keeps its Stop button
 * and its spinner, since the user started it and is owed a way to stop it.
 */
const isLiveRunInFlight = computed(() => props.liveStatus === 'running')

/**
 * True while the IDE is working on something the student asked for: a manual
 * run or a step. A live run is excluded -- the control animates for that.
 */
const isBusy = computed(
  () => (isRunInProgress.value && !isLiveRunInFlight.value) || props.isStepping === true,
)

/** What the control is doing, for the tooltip over the whole of it. */
const runTitle = computed(() => {
  if (props.liveStatus === 'off') return `Run (${appShortcut.run})`
  if (props.liveStatus === 'pending') return 'Live evaluation: about to run'
  if (props.liveStatus === 'running') return 'Live evaluation: running now'
  return 'Live evaluation is on: this file runs when you stop typing'
})

const runMenuItems = computed<MenuItem[]>(() => [
  {
    label: 'Live Evaluation',
    checked: isLive.value,
    run: () => { toggleLiveEvaluation() },
  },
  { separator: true },
  { label: 'Run', kbd: appShortcut.run, run: () => session.execute() },
  // Reachable here as well as from the left half because a *live* run leaves
  // that half showing Run -- so during one this is the only way to stop it
  // short of waiting for the watchdog.
  {
    label: 'Stop',
    disabled: !isRunInProgress.value,
    run: () => { session.stopRun() },
  },
])

/** Where the run menu is open, or null when it is closed. */
const runMenuAt = ref<{ x: number; y: number } | null>(null)

/** Opens the menu under the caret, as the menu bar opens one under its title. */
function toggleRunMenu(e: MouseEvent) {
  if (runMenuAt.value !== null) {
    runMenuAt.value = null
    return
  }
  const rect = (e.currentTarget as HTMLElement).getBoundingClientRect()
  runMenuAt.value = { x: rect.left, y: rect.bottom }
}

const search = ref('')

function searchOpenWindow(searchTerm: string) {
  window.open('docs.html?search=' + encodeURIComponent(searchTerm), '_blank')
}
</script>

<template>
  <!-- The actions worth a single click. Everything the IDE can do, including
       all of these, is in the menu bar above; this row is for the handful
       reached over and over while writing a program. -->
  <div class="ide-header">
    <div class="header-left">
      <button
        type="button"
        class="icon-button fa-solid fa-bars"
        aria-label="Toggle sidebar"
        @click="emit('toggleSidebar')"
      ></button>
      <span class="toolbar-sep" aria-hidden="true"></span>
      <!-- Run, with live evaluation's state on it: "Autorun" while it is on,
           and a stripe crossing the control while a run is coming or going. -->
      <div
        class="run-group"
        :class="{
          'run-group--pending': liveStatus === 'pending',
          'run-group--running': liveStatus === 'running',
        }"
        :title="runTitle"
      >
        <!-- One half swaps between Run and Stop; the pill around it does not,
             so the toolbar keeps its shape while a program runs. -->
        <button
          v-if="isRunInProgress && !isLiveRunInFlight"
          type="button"
          class="icon-button run-main"
          aria-label="Stop"
          @click="session.stopRun()"
        >
          <span class="run-label">
            <i class="fa-solid fa-stop" aria-hidden="true"></i>
          </span>
        </button>
        <!-- The word is the label and the indicator both: nothing else in the
             IDE says that the file is running itself. -->
        <button
          v-else
          type="button"
          class="icon-button run-main"
          accesskey="w"
          :disabled="!props.canRun"
          @click="handleRun"
        >
          <span class="run-label">{{ runLabel }}</span>
        </button>
        <button
          type="button"
          class="icon-button run-caret"
          :class="{ open: runMenuAt !== null }"
          :aria-label="`${runLabel} options`"
          aria-haspopup="menu"
          :aria-expanded="runMenuAt !== null"
          @mousedown.stop
          @click="toggleRunMenu"
        >
          <i class="fa-solid fa-caret-down" aria-hidden="true"></i>
        </button>
      </div>
      <PopupMenu
        v-if="runMenuAt !== null"
        :x="runMenuAt.x"
        :y="runMenuAt.y"
        :items="runMenuItems"
        @close="runMenuAt = null"
      />
      <!-- One spinner for whichever of the two is going, since they would look
           the same anyway. A *live* run is left out: it says so on the control
           itself, and would otherwise put a spinner here as you type. -->
      <span class="spinner-slot" aria-hidden="true">
        <i v-if="isBusy" class="fa-solid fa-spinner fa-spin"></i>
      </span>
      <!-- Steps the statement under the cursor in its own window, rather than
           tracing the whole program in the output pane. -->
      <button
        type="button"
        class="icon-button fa-solid fa-shoe-prints"
        title="Step through the statement under the cursor"
        aria-label="Step statement"
        :disabled="!canStep || isStepping"
        @click="emit('stepStatement')"
      ></button>
      <!-- Opens a REPL on the file as it stands: the same code, somewhere to
           try things against it that the file does not have to keep (#399). -->
      <button
        type="button"
        class="icon-button fa-solid fa-terminal"
        title="Open a REPL on this file"
        aria-label="Open a REPL"
        :disabled="!canRun"
        @click="emit('openRepl')"
      ></button>
      <!-- A query is shown inline in the source, which the notebook is not
           showing, so there is nowhere to put one there (#410). -->
      <button
        type="button"
        class="icon-button fa-solid fa-clipboard-question"
        :title="
          isNotebook
            ? 'Querying a value needs the source view'
            : 'Show the value of the expression under the cursor'
        "
        aria-label="Query value"
        :disabled="isNotebook"
        @click="session.query()"
      ></button>
      <!-- The two ways of looking at the file (#410): as source with its
           output beside it, or as a notebook with the output under each form.
           Held down while the notebook is the one on screen. -->
      <button
        type="button"
        class="icon-button fa-solid fa-book-open"
        :class="{ open: isNotebook }"
        :title="
          isNotebook
            ? 'Show this file as source, with its output beside it'
            : 'Show this file as a notebook'
        "
        aria-label="Notebook view"
        :aria-pressed="isNotebook"
        :disabled="!canRun"
        @click="toggleFileView()"
      ></button>
      <span class="toolbar-sep" aria-hidden="true"></span>
      <input
        v-model="search"
        class="text-input header-search"
        aria-label="Search function name"
        placeholder="Search functions..."
        @keyup.enter="searchOpenWindow(search)"
      />
    </div>
    <div class="header-right">
      <ThemeToggle />
      <ShortcutsHelp />
    </div>
  </div>
</template>

<style scoped>
.ide-header {
  background: var(--header-bg);
  color: var(--header-fg);
  padding: var(--space-xs) var(--space-md);
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  align-items: center;
  gap: var(--space-md);
  z-index: var(--z-header);
}

.header-left,
.header-right {
  display: flex;
  align-items: center;
  gap: var(--space-2xs);
}

/*
 * The left group is what has to give when the window narrows. It used to be the
 * whole row that wrapped while neither group did, so the theme and help buttons
 * dropped to a second line rather than the row reflowing.
 */
.header-left {
  min-width: 0;
  flex: 1;
}

.header-right {
  flex-shrink: 0;
}

/*
 * Was size="30", i.e. ~250-280px of intrinsic width in a group that could not
 * shrink -- the single thing that forced the header to wrap. It now takes what
 * is left and gives it back first.
 */
.header-search {
  flex: 0 1 16rem;
  min-width: 4rem;
}

/*
 * The Run control (#378).
 *
 * A split button: one half runs the file, the other opens the run menu and
 * carries live evaluation's state. They share a single accent pill so the pair
 * reads as one control -- and so the stripe below can cross the whole of it.
 */
.run-group {
  position: relative;
  display: inline-flex;
  align-items: stretch;
  flex-shrink: 0;
  background: var(--accent);
  color: var(--accent-fg);
  border-radius: var(--radius-md);
}

/*
 * The pill draws the fill; its halves are transparent windows onto it, square
 * by default so that only the two ends round off. Each end therefore has to
 * beat this rule outright, which is why both are addressed by two classes
 * below rather than one: at equal specificity they lost to it, and every half
 * computed 0px (#390).
 */
.run-group .icon-button {
  color: inherit;
  background: transparent;
  border-radius: 0;
}

.run-group .icon-button:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.16);
}

/*
 * The half that runs the file, which carries the word.
 *
 * The pill must not change size as that word does, or the toolbar would reflow
 * every time live evaluation was toggled. So the button always reserves the
 * width of the longer of the two with a hidden copy of it, and the real label
 * -- "Run", "Autorun", or the Stop icon -- is laid over the top.
 */
.run-group .icon-button.run-main {
  position: relative;
  padding-inline: var(--space-md);
  font-size: var(--text-sm);
  font-weight: 600;
  border-radius: var(--radius-md) 0 0 var(--radius-md);
}

.run-main::after {
  content: 'Autorun';
  visibility: hidden;
}

.run-label {
  position: absolute;
  inset: 0;
  display: flex;
  align-items: center;
  justify-content: center;
}

.run-group .icon-button.run-caret {
  padding-inline: var(--space-sm);
  border-radius: 0 var(--radius-md) var(--radius-md) 0;
  /* A hairline rather than a gap: the two halves must not drift apart. */
  border-left: 1px solid rgba(255, 255, 255, 0.28);
}

.run-caret.open {
  background: rgba(255, 255, 255, 0.16);
}

/*
 * The spinner's place in the row, kept whether or not it is in it: appearing
 * and disappearing, it would otherwise shove the rest of the toolbar sideways
 * every time a program ran. Sized in em so it follows the icons around it.
 */
.spinner-slot {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex-shrink: 0;
  width: 1.25em;
}

/*
 * The stripe. An overlay rather than a background on the pill itself, so it
 * can be animated without disturbing the fill, and clipped to the pill's own
 * corners rather than by `overflow: hidden` -- which would cut the focus ring
 * off either half.
 */
.run-group::after {
  content: '';
  position: absolute;
  inset: 0;
  border-radius: inherit;
  pointer-events: none;
  opacity: 0;
  transition: opacity 150ms ease-out;
}

/*
 * Waiting out the pause after a keystroke: a slow breath, which is as much as
 * a state that lasts 750ms should ask for.
 */
.run-group--pending::after {
  opacity: 1;
  background: rgba(255, 255, 255, 0.18);
  animation: run-breathe 1.2s ease-in-out infinite;
}

/*
 * Running: barber-pole stripes crossing the pill. Indeterminate on purpose --
 * how long a student's program will take is not knowable, and a bar that
 * pretended to know would be lying.
 */
.run-group--running::after {
  opacity: 1;
  background-image: repeating-linear-gradient(
    115deg,
    rgba(255, 255, 255, 0) 0 0.5rem,
    rgba(255, 255, 255, 0.3) 0.5rem 1rem
  );
  background-size: 2rem 100%;
  animation: run-stripes 700ms linear infinite;
}

@keyframes run-breathe {
  50% {
    opacity: 0.35;
  }
}

@keyframes run-stripes {
  to {
    background-position: 2rem 0;
  }
}

/*
 * Motion is the whole point of the indicator, so it is replaced rather than
 * dropped: a steady wash still says a run is in hand.
 */
@media (prefers-reduced-motion: reduce) {
  .run-group--pending::after,
  .run-group--running::after {
    animation: none;
    background-image: none;
    background: rgba(255, 255, 255, 0.2);
  }
}
</style>
