<script setup lang="ts">
import { ref } from "vue"
import { processDocs } from "./readDocs.js"

//import { Doc } from "./api/docs.js"
//import DocEntry from "./DocEntry.vue"

const options = ref([
  { id: 1, val: 'notes' },
  { id: 2, val: 'half' },
  { id: 3, val: 'whole' }
]);
const isOpen = ref(false)
const tags = ref([])
const types = ref([
  { id: 1, val: 'number'},
  { id: 2, val: 'string' },
  { id: 3, val: 'boolean' }
])

const search = ref("")

function searchComplex() {
  //grab all info and do search
}

</script>

<!-- Tag selection goes here -->
<template>
  <div class="index">
    <h3> <strong>Search Characteristics</strong> </h3>
    <p></p>

    <p>Keywords</p>
    <input
      v-model="search"  
      size="24"
      class="search-input"
      placeholder="Function name/keywords"
    >

    <p>Types</p>
    <label v-for="o in types" :key="o.id">
        <p><input type="checkbox" class="indent" :value= "o.val" v-model="types">
        {{ o.val }}</p>
    </label>

    <p>Tags:{{ tags }}</p>

    <p> </p>
      <text>Music </text>
      <button  @click="isOpen = !isOpen">{{ isOpen ? "▼" : "▶" }}</button>

      <div v-if="isOpen" class="dropdown-menu">
      <label v-for="o in options" :key="o.id">
        <p><input type="checkbox" class="indent" :value= "o.val" v-model="tags">
        {{ o.val }}</p>
      </label>
    </div>

      <!-- <Dropdown v-model="tags"> -->
      <p><input type="checkbox" v-model="tags" label="One" value="One"> One </p>
      <p><input type="checkbox" v-model="tags" label="Two" value="Two"> Two</p>
      <p><input type="checkbox" v-model="tags" label="Three" value="Three"> Three </p>
      <!-- </Dropdown> -->

      <button @click="processDocs">Enter</button>
  </div>



    <!-- <select v-model="tags" multiple>
        <option value="volvo">Volvo</option>
        <option value="saab">Saab</option>
        <option value="mercedes">Mercedes</option>
        <option value="audi">Audi</option>
    </select> -->
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

.indent {
  margin-left: 24px;
}

.search-input {
  font: Roboto, "Helvetica Neue", Arial, sans-serif;
}

</style>
