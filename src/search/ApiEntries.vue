<script setup lang="ts">
import { ref } from "vue"

//import { Doc } from "./api/docs.js"
//import DocEntry from "./DocEntry.vue"


const tags = ref([])
</script>

<!-- Tag selection goes here -->
<template>
  <div class="index">
    <p>{{ tags }}</p>
      <p><input type="checkbox" v-model="tags" label="One" value="One"> One </p>
      <p><input type="checkbox" v-model="tags" label="Two" value="Two"> Two</p>
  <p><input type="checkbox" v-model="tags" label="Three" value="Three"> Three </p>
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
