<script setup lang="ts">
import { ref } from 'vue'
import ApiEntries from './ApiEntries.vue'
const appVersion = APP_VERSION

const search = ref('')

function searchForFunction(searchTerm: string) {
  window.open('/src/app/search/search.html?search=' + encodeURIComponent(searchTerm), '_self')
}

const urlParams = new URLSearchParams(window.location.search)
const searched: string | null = urlParams.get('search')
</script>

<template>
  <div class="docs-root">
    <div class="header">
      <div>
        <a href="index.html">Scamper</a> <span>({{ appVersion }})</span> ⋅
        <a href="docs.html">Docs</a> ⋅
        <a href="reference.html">Reference</a> ⋅
        <a href="search.html">Search</a>

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
    <div class="flex-box">
      <h2 :style="{  }">Search results {{(searched)? "for " + searched : ''}}</h2>
      <input
        v-model="search"  
        size = "30"
        class="search-input"
        placeholder="Search function or press enter..."
        @keyup.enter="searchForFunction(search)"
      >
    </div>
    <div :style="{ margin: '15px' }">
      <ApiEntries :search-in="searched || ''"/>
    </div>
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
  background: #ffacac;
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

.flex-box {
  display: flex;
  flex-direction: row;
  gap: 30px;
  margin-top: -10px;
  margin-bottom: -10px;
}

.flex-box2 {
  display: flex;
  flex-direction: row;
  gap: 30px;
}

.search-input{
  height: 24px;
  margin-top: 20px;
}
</style>
