<script setup lang="ts">
import { computed, ref } from 'vue'
import { useScamperSession } from '../composables/use-scamper-session'
import ThemeToggle from '../../shared/ThemeToggle.vue'
import ShortcutsHelp from './ShortcutsHelp.vue'

defineProps<{
  currentFile?: string | null
  /** False when the cursor is not inside a statement, so there is none to step. */
  canStep?: boolean
  /** True while a trace is being collected, which takes a moment. */
  isStepping?: boolean
}>()

const emit = defineEmits<{
  toggleSidebar: []
  stepStatement: []
}>()

const session = useScamperSession()

const isRunInProgress = computed(() => session.currentRun.value !== null)

async function handleRun() {
  await session.execute()
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
      <template v-if="isRunInProgress">
        <button
          type="button"
          class="icon-button fa-solid fa-stop"
          aria-label="Stop"
          @click="session.stopRun()"
        ></button>
        <i class="fa-solid fa-spinner fa-spin" aria-hidden="true"></i>
      </template>
      <button
        v-else
        type="button"
        class="icon-button icon-button--accent fa-solid fa-play"
        aria-label="Run"
        accesskey="w"
        @click="handleRun"
      ></button>
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
</style>
