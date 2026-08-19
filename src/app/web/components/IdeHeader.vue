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
  runWindow: []
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
        class="fa fa-bars"
        aria-label="Toggle sidebar"
        @click="emit('toggleSidebar')"
      ></button>
      ⋅
      <template v-if="isRunInProgress">
        <button
          class="fa-solid fa-stop"
          aria-label="Stop"
          @click="session.stopRun()"
        ></button>
        <i class="fa-solid fa-spinner fa-spin"></i>
      </template>
      <button
        v-else
        class="fa-solid fa-play"
        aria-label="Run"
        accesskey="w"
        aria-keyshortcuts="w"
        @click="handleRun"
      ></button>
      <!-- Steps the statement under the cursor in its own window, rather than
           tracing the whole program in the output pane. -->
      <button
        class="fa-solid fa-shoe-prints"
        title="Step through the statement under the cursor"
        aria-label="Step statement"
        :disabled="!canStep || isStepping"
        @click="emit('stepStatement')"
      ></button>
      <i v-if="isStepping" class="fa-solid fa-spinner fa-spin"></i>
      <button
        class="fa-solid fa-window-maximize"
        aria-label="Maximize Output Window"
        :disabled="!currentFile"
        @click="emit('runWindow')"
      ></button>
      <button
        class="fa-solid fa-clipboard-question"
        aria-label="Query value"
        @click="session.query()"
      ></button>
      ⋅
      <input
        v-model="search"
        size="30"
        aria-label="Search function name"
        placeholder="Search function name or press enter..."
        @keyup.enter="searchOpenWindow(search)"
      >
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
  padding: 0.5em;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: space-between;
  align-items: center;
  z-index: 2;
}

.header-left,
.header-right {
  display: flex;
  align-items: center;
  gap: 0.25em;
}
</style>
