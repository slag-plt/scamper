<script setup lang="ts">
import { computed } from 'vue'
import type { FunctionDoc } from '../../scheme/docstring/docstring'
import type { ModuleDoc } from '../../scheme/docstring/module-doc'
import DocEntry from './DocEntry.vue'
import DocText from './DocText.vue'

const props = defineProps<{
  moduleName: string
  lib: Map<string, FunctionDoc>
  /**
   * What the module is for, if it says (#411). Absent for a module with no
   * module comment, which renders nothing rather than an empty heading.
   */
  moduleDoc?: ModuleDoc
}>()

interface Entry {
  id: string
  name: string
  doc: FunctionDoc
}

function entryId(module: string, name: string): string {
  return `${module}-${name}`
}

const entries = computed<Entry[]>(() => {
  return [...props.lib.entries()].map(([name, doc]) => ({
    id: entryId(props.moduleName, name),
    name,
    doc,
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
      <!-- What the module is for, above what is in it. Only when it says: a
           module with no module comment shows nothing here at all. -->
      <p v-if="moduleDoc !== undefined" class="module-doc">
        <DocText :text="moduleDoc.description" />
      </p>
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
  background-color: var(--surface-muted);
  font-family: var(--font-mono);
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

/* Aligned with the entry cards below it, and set apart from them: prose about
   the module rather than another entry in it. */
.module-doc {
  margin: 1em;
  padding: 0 1em;
  font-family: var(--font-sans);
  font-size: 1.05em;
  line-height: 1.5;
}
</style>
