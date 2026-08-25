<script setup lang="ts">
import { computed, ref } from 'vue'
import { useScamperSession } from '../composables/use-scamper-session'
import type { MenuItem } from '../menu'
import type { LiveStatus } from '../composables/use-live-evaluation'
import { toggleLiveEvaluation } from '../run-prefs'
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
  }>(),
  { liveStatus: 'off' },
)

const emit = defineEmits<{
  toggleSidebar: []
  stepStatement: []
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
 * The word "Auto" is on it whenever live evaluation is on, and the control
 * animates while a run is coming or going -- which is the only sign a student
 * gets that the output they are watching is about to be replaced.
 */

/** True when live evaluation is on, whatever it happens to be doing. */
const isLive = computed(() => props.liveStatus !== 'off')

/**
 * True while a live run is coming or in flight, i.e. what animates.
 *
 * A *manual* run is deliberately not included: it swaps the left half for a
 * Stop button, which is indication enough, and animating it would say that
 * Scamper had started something the user did not.
 */
const isLiveWorking = computed(
  () => props.liveStatus === 'pending' || props.liveStatus === 'running',
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
  window.open('search.html?search=' + encodeURIComponent(searchTerm), '_blank')
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
      <!-- Run, with live evaluation's state on it: "Auto" while it is on, and
           a stripe crossing the control while a run is coming or going. -->
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
          v-if="isRunInProgress && !isLiveWorking"
          type="button"
          class="icon-button run-main fa-solid fa-stop"
          aria-label="Stop"
          @click="session.stopRun()"
        ></button>
        <button
          v-else
          type="button"
          class="icon-button run-main fa-solid fa-play"
          aria-label="Run"
          accesskey="w"
          @click="handleRun"
        ></button>
        <button
          type="button"
          class="icon-button run-caret"
          :class="{ open: runMenuAt !== null }"
          :aria-label="isLive ? 'Auto run options' : 'Run options'"
          aria-haspopup="menu"
          :aria-expanded="runMenuAt !== null"
          @mousedown.stop
          @click="toggleRunMenu"
        >
          <!-- The label is the indicator: no other part of the IDE says that
               the file is running itself. -->
          <span v-if="isLive" class="run-auto">Auto</span>
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
      <!-- A manual run says so here; a live one says so on the control itself,
           which would otherwise put a spinner in the toolbar as you type. -->
      <i
        v-if="isRunInProgress && !isLiveWorking"
        class="fa-solid fa-spinner fa-spin"
        aria-hidden="true"
      ></i>
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
      <i
        v-if="isStepping"
        class="fa-solid fa-spinner fa-spin"
        aria-hidden="true"
      ></i>
      <button
        type="button"
        class="icon-button fa-solid fa-clipboard-question"
        title="Show the value of the expression under the cursor"
        aria-label="Query value"
        @click="session.query()"
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

/* The pill draws the fill; its halves are transparent windows onto it. */
.run-group .icon-button {
  color: inherit;
  background: transparent;
  border-radius: 0;
}

.run-group .icon-button:hover:not(:disabled) {
  background: rgba(255, 255, 255, 0.16);
}

.run-main {
  border-radius: var(--radius-md) 0 0 var(--radius-md);
}

.run-caret {
  gap: var(--space-2xs);
  padding-inline: var(--space-sm);
  border-radius: 0 var(--radius-md) var(--radius-md) 0;
  /* A hairline rather than a gap: the two halves must not drift apart. */
  border-left: 1px solid rgba(255, 255, 255, 0.28);
}

.run-caret.open {
  background: rgba(255, 255, 255, 0.16);
}

/*
 * The word that says the file is running itself. Small and quiet: it is a
 * status, and the student's attention belongs in the editor.
 */
.run-auto {
  font-size: var(--text-sm);
  font-weight: 600;
  line-height: 1;
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
