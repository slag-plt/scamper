<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref } from 'vue'
import ModuleList from './ModuleList.vue'
import ApiEntries from './ApiEntries.vue'
import SearchResults from './SearchResults.vue'
import ThemeToggle from '../shared/ThemeToggle.vue'
import { docRegistry, moduleDocRegistry } from '../../lib'
import { moduleOrder } from './modules'
import type { SearchRequest } from './search'
import type { FunctionDoc } from '../../scheme/docstring/docstring'

const appVersion = APP_VERSION

const libs: [string, Map<string, FunctionDoc>][] = moduleOrder.map(
  (name) => [name, docRegistry.get(name) ?? new Map<string, FunctionDoc>()],
)

const selectedModule = ref('prelude')

const selectedLib = computed(
  () =>
    libs.find(([name]) => name === selectedModule.value)?.[1] ??
    new Map<string, FunctionDoc>(),
)

/** What the selected module is for, if it says (#411). */
const selectedModuleDoc = computed(() =>
  moduleDocRegistry.get(selectedModule.value),
)

/*
 * Which view is showing lives in the URL rather than in a toggle, because
 * search used to be its own page and links to it are still out there (#403).
 * `?search=` present, empty or not, means search; absent means the modules.
 */
function termFromUrl(): string | null {
  return new URLSearchParams(window.location.search).get('search')
}

const searchTerm = ref(termFromUrl())
const request = ref<SearchRequest>({ term: searchTerm.value ?? '' })
const search = ref(searchTerm.value ?? '')
const searching = computed(() => searchTerm.value !== null)

/**
 * Shows the search view for `term`, or the module browser for null. A change
 * of view is a history entry, so Back still leaves search the way it always
 * did; repeating the search already showing is not.
 */
function show(term: string | null) {
  const changed = term !== searchTerm.value
  searchTerm.value = term
  search.value = term ?? ''
  if (term !== null) {
    request.value = { term }
  }
  if (!changed) {
    return
  }
  const url = new URL(window.location.href)
  if (term === null) {
    url.searchParams.delete('search')
  } else {
    url.searchParams.set('search', term)
  }
  window.history.pushState({}, '', url)
}

function syncFromUrl() {
  const term = termFromUrl()
  searchTerm.value = term
  search.value = term ?? ''
  request.value = { term: term ?? '' }
}

onMounted(() => {
  window.addEventListener('popstate', syncFromUrl)
})
onUnmounted(() => {
  window.removeEventListener('popstate', syncFromUrl)
})
</script>

<template>
  <div class="docs-root">
    <div class="header">
      <div class="header-left">
        <a href="index.html">Scamper</a> <span>({{ appVersion }})</span> ⋅
        <a href="docs.html">Docs</a> ⋅
        <a href="reference.html">Reference</a> ⋅
        <input
          v-model="search"
          class="text-input"
          size="30"
          aria-label="Search function name"
          placeholder="Search function name or press enter..."
          @keyup.enter="show(search)"
        />
        <button
          v-if="searching"
          type="button"
          class="back"
          @click="show(null)"
        >
          Back to modules
        </button>
      </div>
      <div class="header-right">
        <ThemeToggle />
        ⋅
        <a href="https://github.com/slag-plt/scamper"
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
    <div class="docs">
      <SearchResults v-if="searching" :request="request" />
      <template v-else>
        <ModuleList
          :libs="libs"
          :selected-module="selectedModule"
          @select="selectedModule = $event"
        />
        <ApiEntries
          :module-name="selectedModule"
          :lib="selectedLib"
          :module-doc="selectedModuleDoc"
        />
      </template>
    </div>
  </div>
</template>

<style>
html,
body,
#app {
  width: 100%;
  height: 100%;
  margin: 0;
  padding: 0;
  font-family: var(--font-sans);
  font-size: 1em;
}
</style>

<style scoped>
.docs-root {
  height: 100%;
  display: flex;
  flex-direction: column;
}

.header {
  background: var(--header-bg);
  color: var(--header-fg);
  padding: var(--space-md);
  flex: 0 0 auto;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: space-between;
  gap: var(--space-md);
}

.header-left,
.header-right {
  color: var(--header-fg);
  display: flex;
  align-items: center;
  gap: var(--space-xs);
}

.back {
  padding: var(--space-xs) var(--space-md);
  font: inherit;
  font-size: var(--text-md);
  color: inherit;
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  cursor: pointer;
}

.back:hover {
  background: var(--surface-hover);
}

.docs {
  display: flex;
  flex-direction: column;
  flex: 1;
  min-height: 0;
}
</style>
