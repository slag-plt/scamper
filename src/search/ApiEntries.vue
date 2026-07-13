<script setup lang="ts">
import { reactive, Ref, ref, h } from "vue"
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

//let filteredLibs: Ref<any[]> = ref(libs.splice(0, libs.length - 1))

let filteredLibs = ref<any[]>([])

function setSimple() {
  filteredLibs.value = []
  const simpleLibs: any[] = []
  Object.entries(libs).forEach(([,[, lib]]) => {
    Object.entries(lib).forEach(([n, foo]) => {
      simpleLibs.push(foo)
    })
  });
  
  filteredLibs.value = simpleLibs
  console.log("this is where joy comes to die", filteredLibs)
}

function checkReturn(foo: string | object) {
  let boo = false;

  Object.entries(foo).forEach(([name, type]) => {
    if (name === "returnType") {
      type = type.replace("?", "")
      if(returnTypes.value.includes(type)) {
        boo = true;
      }
    }
  })
  return boo
}

function checkArgs(foo: string | object) {
  let boo = false;

  console.log("foo", foo)

  Object.entries(foo).forEach(([name, argArr]) => {
    if (name === "args") {
      console.log("argArr", argArr)

      Object.entries(argArr).forEach(([,arg]) => {
        console.log("arg", arg)

        Object.entries(arg).forEach(([key, value]) => {
          console.log("key", key, "value", value)

          if (key === "desc") {
            if (typeof value === "string") {
              console.log("this is a string", value)
              value = value.replace("?", "")
            }
            if(argumentTypes.value.includes(value as string)) {
              console.log("it is included", value,argumentTypes.value.includes(value as string) )
              boo = true;
            }
          }
        })
        
        // Object.entries(arg).forEach((key, value) => {
        //   if (key === "desc") {
        //     value = value.replace("?", "")
        //     if(argumentTypes.value.includes(value)) {
        //       boo = true;
        //     }
        //   }
        // })
        
        // arg = arg.replace("?", "")
        // if(argumentTypes.value.includes(arg)) {
        //   boo = true;
        // }
      })
    }
  })
  return boo
}

function checkTags(foo: [string, string | object]) {
  let boo = true;

  // const tagType = typeof foo[1] === 'object' ? (foo[1] as { tags?: string }).tags : undefined;
  // Object.entries(tags.value).forEach(([tag]) => {
    
  // })
  return boo;
}

function addToLib() {
  const newArr: any[] = []

  Object.entries(libs).forEach(([, [, lib]]) => {
    Object.entries(lib).forEach(([, foo]) => {
      console.log("checkReturn(foo) || checkArgs(foo)", checkReturn(foo) || checkArgs(foo))
      console.log("checkReturn(foo)", checkReturn(foo))
      console.log("checkArgs(foo)", checkArgs(foo))
      if (checkReturn(foo) || checkArgs(foo)) {
        newArr.push(foo)
      }
    })
  });

  filteredLibs.value = newArr
}

const options = ref([
  { id: 1, val: 'notes' },
  { id: 2, val: 'half' },
  { id: 3, val: 'whole' }
]);

const AisOpen = ref(false)
const RisOpen = ref(false)
const TisOpen = ref(false)

const tags = ref<string[]>([])
const argumentTypes = ref<string[]>([])
const returnTypes = ref<string[]>([])
const pBool = ref([
{ id: 7, val: 'or' },
{ id: 8, val: 'and' }
])

const types = ref([
  { id: 4, val: 'number'},
  { id: 5, val: 'string' },
  { id: 6, val: 'boolean' }
])


function makeString(foo: string): string {
  let str = ""
  for (const [key, value] of Object.entries(foo)) {
    str += `${key}: ${value}\n`     
  }
  return str + "\n"
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
      <p><input v-model="tags" type="checkbox" value="Music"> Music </p>
      <button class="arrow-button" @click="TisOpen = !TisOpen">{{ TisOpen ? "▼" : "▶" }}</button>
    </div>
        <div v-if="TisOpen" class="dropdown-menu">
          <label v-for="e in options" :key="e.id">
            <div><p><input v-model="tags" type="checkbox" class="indent" :value= "e.val" >
            {{ e.val }}</p></div>
          </label>
        </div>

      <!-- <Dropdown v-model="tags"> -->
      <p><input v-model="tags" type="checkbox"  label="One" value="One"> One </p>
      <p><input v-model="tags" type="checkbox" label="Two" value="Two"> Two</p>
      <p><input v-model="tags" type="checkbox" label="Three" value="Three"> Three </p>
      <!-- </Dropdown> -->
  
    </div>
      <button class="enter-button" @click="() => { addToLib() }"><strong>Enter</strong></button>
    </div>

    <div>
      <h3> <strong>Search Results</strong> </h3>
                <div class="api">
                  <div class="index2"> 
                    <div class="entries"> {{(filteredLibs.length === 0)? setSimple() : null}}
                        <div v-for="(foo) in filteredLibs" :key="foo['name']" ref="foo['name']"> 
                          <text>{{ makeString(foo) }}</text>
                        </div>
                    </div>
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
  height: 600px;
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
  white-space: pre-line;
}

</style>
