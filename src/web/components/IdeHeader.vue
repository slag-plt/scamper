<script setup lang="ts">
import { computed, ref } from "vue"
import { useScamperSession } from "../composables/use-scamper-session"

defineProps<{
  currentFile?: string | null
}>()

const emit = defineEmits<{
  runWindow: []
  toggleSidebar: []
}>()

const session = useScamperSession()

const isRunInProgress = computed(() => session.currentRun.value !== null)

async function handleRun() {
  await session.execute()
}

function handleTrace() {
  void session.execute({ tracing: true })
}

const search = ref("")

function searchOpenWindow(searchTerm: string) {
  window.open("search.html?search=" + encodeURIComponent(searchTerm), "_blank")
}
</script>

<template>
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
      <button
        class="fa-solid fa-route"
        aria-label="Trace"
        @click="handleTrace"
      ></button>
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
      <!-- TODO: re-enable once AST is migrated to new backend -->
      <button
        class="fa-solid fa-tree"
        aria-label="Display Syntax Tree"
        disabled
      ></button>
      ⋅
      <a href="docs.html">Docs</a>
      ⋅
      <a href="reference.html">Reference</a>
      ⋅
      <input
        v-model="search"  
        size = "30"
        placeholder="Search function or press enter..."
        @keyup.enter="searchOpenWindow(search)"
      >
    </div>
    <div class="header-right">
      <a
        href="https://github.com/slag-plt/scamper"
        role="button"
        aria-label="Scamper Repository"
        ><i class="fa-brands fa-github"></i
      ></a>
      ⋅
      <em
        ><a href="https://github.com/slag-plt/scamper/issues"
          >Report an issue</a
        ></em
      >
    </div>
  </div>
</template>

<style scoped>
.ide-header {
  background: #eee;
  color: #333;
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
