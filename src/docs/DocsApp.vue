<script setup lang="ts">
import { computed, ref } from 'vue'

const appVersion = APP_VERSION
import ModuleList from './ModuleList.vue'
import ApiEntries from './ApiEntries.vue'
import { docRegistry } from '../lib'
import type { FunctionDoc } from '../scheme/docstring/docstring'

// N.B., order matches the old hand-maintained api/*.ts file list (prelude
// first as the default/most common module); "runtime" is LPM-internal
// plumbing, not user-facing, so it's deliberately excluded here.
const moduleOrder = [
  'prelude', 'image', 'lab', 'music', 'test',
  'audio', 'canvas', 'html', 'reactive', 'data', 'rex',
]
const libs: [string, Map<string, FunctionDoc>][] = moduleOrder.map(
  (name) => [name, docRegistry.get(name) ?? new Map<string, FunctionDoc>()],
)

const selectedModule = ref('prelude')

const selectedLib = computed(
  () =>
    libs.find(([name]) => name === selectedModule.value)?.[1] ??
    new Map<string, FunctionDoc>(),
)

const search = ref('')

function searchForFunction(searchTerm: string) {
  window.open('search.html?search=' + encodeURIComponent(searchTerm), '_self')
}
</script>

<template>
  <div class="docs-root">
    <div class="header">
      <div>
        <a href="index.html">Scamper</a> <span>({{ appVersion }})</span> ⋅
        <a href="docs.html">Docs</a> ⋅
        <a href="reference.html">Reference</a> ⋅
        <a href="search.html">Search</a> ⋅ 

        <input
      v-model="search"  
      size = "30"
      placeholder="Search function or press enter..."
      @keyup.enter="searchForFunction(search)"
        >

      </div>
      <div class="header-right">
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
      <ModuleList
        :libs="libs"
        :selected-module="selectedModule"
        @select="selectedModule = $event"
      />
      <ApiEntries :module-name="selectedModule" :lib="selectedLib" />
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
  font-family:
    -apple-system,
    BlinkMacSystemFont,
    avenir next,
    avenir,
    segoe ui,
    helvetica neue,
    helvetica,
    Cantarell,
    Ubuntu,
    roboto,
    noto,
    arial,
    sans-serif;
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
  background: #eee;
  color: #333;
  padding: 0.5em;
  flex: 0 0 auto;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: space-between;
}

.header-right {
  color: #333;
}

.docs {
  display: flex;
  flex-direction: column;
  flex: 1;
  min-height: 0;
}
</style>
