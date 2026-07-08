<script setup lang="ts">
import { ref } from "vue"
import { toRaw, isProxy } from 'vue';


import { computed } from "vue"
import { Doc } from "./api/docs.js"
import DocEntry from "./DocEntry.vue"
//import libs from "./DocsApp.vue"
//import { processDocs } from "./readDocs.js"

//import { Doc } from "./api/docs.js"
//import DocEntry from "./DocEntry.vue"

import * as Audio from "./api/audio.js"
import * as Prelude from "./api/prelude.js"
import * as Image from "./api/image.js"
import * as Lab from "./api/lab.js"
import * as Music from "./api/music.js"
import * as Test from "./api/test.js"
import * as Canvas from "./api/canvas.js"
import * as Html from "./api/html.js"
import * as Reactive from "./api/reactive.js"
import * as Data from "./api/data.js"
import * as Rex from "./api/rex.js"

const libs: [string, object][] = [
  ["prelude", Prelude],
  ["image", Image],
  ["lab", Lab],
  ["music", Music],
  ["test", Test],
  ["audio", Audio],
  ["canvas", Canvas],
  ["html", Html],
  ["reactive", Reactive],
  ["data", Data],
  ["rex", Rex],
]

const options = ref([
  { id: 1, val: 'notes' },
  { id: 2, val: 'half' },
  { id: 3, val: 'whole' }
]);

const AisOpen = ref(false)
const RisOpen = ref(false)
const TisOpen = ref(false)

const tags = ref([])
const argumentTypes = ref([])
const returnTypes = ref([])
const pBool = ref([
{ id: 7, val: 'or' },
{ id: 8, val: 'and' }
])

const types = ref([
  { id: 4, val: 'number'},
  { id: 5, val: 'string' },
  { id: 6, val: 'boolean' }
])

const rawLibs = toRaw(libs);

function makeString(foo: object) {
  let str = "\n"
  for (const [key, value] of Object.entries(foo)) {
    str += `${key}: ${value}\n`     
  }
  return str
}

function checkReturn(foo: object) {
  return true;
}

function checkArg(foo: object) {
  return true;
}

function searchFilter(foo: object) {
  if(checkReturn(foo) && checkArg(foo)) return true;
  return false;
}


</script>

<!-- Tag selection goes here -->
<template>

  <div class="flex-box">

    <div>

  <h3> <strong>Search by Type</strong> </h3>
  <div class="index">

    <text>Search Boolean </text>
    <select>
      <option v-for="o in pBool" :key="o.id">
        <p><input v-model="argumentTypes" type="checkbox" class="indent" :value= "o.val" >
        {{ o.val }}</p>
      </option>
    </select>

    <!-- <p>Keywords</p>
    <input
      v-model="search"  
      size="24"
      class="search-input"
      placeholder="Function name/keywords"
    > -->
    <p></p>
   
      <text>Arguments </text>
      <button class="arrow-button" @click="AisOpen = !AisOpen">{{ AisOpen ? "▼" : "▶" }}</button>
        <div v-if="AisOpen" class="dropdown-menu">
          <label v-for="o in types" :key="o.id">
            <p><input v-model="argumentTypes" type="checkbox" class="indent" :value= "o.val" >
            {{ o.val }}</p>
          </label>
        </div>
         <p>Types:{{ argumentTypes }}</p>
    
      <text>Return </text>
      <button class="arrow-button" @click="RisOpen = !RisOpen">{{ RisOpen ? "▼" : "▶" }}</button>
        <div v-if="RisOpen" class="dropdown-menu">
          <label v-for="o in types" :key="o.id">
            <p><input v-model="returnTypes" type="checkbox" class="indent" :value= "o.val" >
            {{ o.val }}</p>
          </label>
        </div>
        <p>Types:{{ returnTypes }}</p>

  </div>

  <h3> <strong>Search by Tag</strong> </h3>
  <div class="index">

    <text>Search Boolean </text>
    <select>
      <option v-for="o in pBool" :key="o.id">
        <div><p><input v-model="argumentTypes" type="checkbox" class="indent" :value= "o.val" >
        {{ o.val }}</p></div>
      </option>
    </select>


    <p>Tags:{{ tags }}</p>

    <p> </p>
    <div class="container">
      <p><input type="checkbox" v-model="tags" value="Music"> Music </p>
      <button class="arrow-button" @click="TisOpen = !TisOpen">{{ TisOpen ? "▼" : "▶" }}</button>
    </div>
        <div v-if="TisOpen" class="dropdown-menu">
          <label v-for="e in options" :key="e.id">
            <div><p><input v-model="tags" type="checkbox" class="indent" :value= "e.val" >
            {{ e.val }}</p></div>
          </label>
        </div>

      <!-- <Dropdown v-model="tags"> -->
      <p><input type="checkbox" v-model="tags" label="One" value="One"> One </p>
      <p><input type="checkbox" v-model="tags" label="Two" value="Two"> Two</p>
      <p><input type="checkbox" v-model="tags" label="Three" value="Three"> Three </p>
      <!-- </Dropdown> -->
  </div>

  <button class="enter-button" @click="searchFilter"><strong>Enter</strong></button>

    <!-- <select v-model="tags" multiple>
        <option value="volvo">Volvo</option>
        <option value="saab">Saab</option>
        <option value="mercedes">Mercedes</option>
        <option value="audi">Audi</option>
    </select> -->
    </div>

    <div>
      <h3> <strong>Search Results</strong> </h3>
                     <div class="api">
                  <div class="index2"> 
                    <!-- <ul> -->
                    <div class="entries">
                      <div v-for="library in rawLibs" :key="library[0]">
                        <text><strong>{{library[0]}}</strong></text>
                        <div v-for="foo in library[1]" :key="foo[0]">
                          <text v-if="1" >{{makeString(foo)}}</text>
                        </div>
                      </div>

                    </div>
                    <!-- </ul> -->
                  </div>
                </div> 
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

.index2 {
  margin: 1em;
  padding: 1em;
  font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
  flex-shrink: 0;
  min-height: 0;
  width: 90%;
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

.container {
  display: flex;
  gap: 10px;
  margin-bottom: -20px;
  margin-top: -20px;
}

.arrow-button {
  height: 24px;
  margin-top: 11px;
}

.enter-button {
  height: 88px;
  width: 188px;
  margin: 24px;
  font-size: 40px;
  font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
  color: #ff8080;
}

.flex-box {
  display: flex;
  flex-direction: row;
  gap: 70px;
}

.entries {
  flex: 1;
  min-height: 0;
  white-space: pre-line
}

</style>
