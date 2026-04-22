<script setup lang="ts">
import { computed } from "vue"
import { Doc } from "./api/docs.js"
import DocEntry from "./DocEntry.vue"

const props = defineProps<{
  moduleName: string
  lib: object
}>()

interface Entry {
  id: string
  name: string
  doc: Doc
}

function entryId(module: string, name: string): string {
  return `${module}-${name}`
}

const entries = computed<Entry[]>(() => {
  return Object.entries(props.lib).map(([name, doc]) => ({
    id: entryId(props.moduleName, name),
    name: (doc as Doc).name,
    doc: doc as Doc,
  }))
})
</script>

<template>
  <div class="api">
    <div class="index">
      <strong>{{ moduleName }}</strong>
      <ul>
        <li v-for="entry in entries" :key="entry.id">
          <a :href="`#${entry.id}`">{{ entry.name }}</a>
        </li>
      </ul>
    </div>
    <div class="entries">
      <DocEntry
        v-for="entry in entries"
        :id="entry.id"
        :key="entry.id"
        :doc="entry.doc"
      />
    </div>
  </div>
</template>

<style scoped>
.api {
  display: flex;
  flex-direction: row;
  width: 100%;
  flex: 1;
  min-height: 0;
}

.index {
  margin: 1em;
  padding: 1em;
  background-color: #ddd;
  font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
  width: 17em;
  flex-shrink: 0;
  min-height: 0;
  overflow: scroll;
}

.index ul,
.index li {
  list-style-type: none;
  list-style-position: inside;
  margin: 0;
  padding: 0;
}

.entries {
  flex: 1;
  min-height: 0;
  overflow: scroll;
}
</style>
