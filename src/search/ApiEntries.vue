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


// function checkReturnAnd(foo: string | object) {
//   const tempArr: any[] = []

//   Object.entries(foo).forEach(([name, type]) => {
//     if (name === "returnType") {
//       type = type.replace("?", "")
//       tempArr.push(type)
//       // if(!returnTypes.value.includes(type)) {
//       //   boo = false;
//       // }
//     }
//   })

//   let boo = true;

//   returnTypes.value.forEach((arrType) => {
//     if(!tempArr.includes(arrType)) {
//       boo = false
//     }
//   })
//   return boo
// }


function checkArgs(foo: string | object) {
  let boo = false;

  Object.entries(foo).forEach(([name, argArr]) => {
    if (name === "args") {
      //console.log("argArr", argArr)

      Object.entries(argArr).forEach(([,arg]) => {
        //console.log("arg", arg)
      
        Object.entries(arg).forEach(([key, value]) => {
          //console.log("key", key, "value", value)

          if (key === "desc") {
            if (typeof value === "string") {
              //console.log("this is a string", value)
              value = value.replace("?", "")
            }
            if(argumentTypes.value.includes(value as string)) {
              //console.log("it is included", value,argumentTypes.value.includes(value as string) )
              boo = true;
            }
          }
        })
      })
    }
  })
  return boo
}


function checkArgsAnd(foo: string | object) {
  //console.log("foo", foo)
  let tempArr: any[] = []

  Object.entries(foo).forEach(([name, argArr]) => {
    if (name === "args") {
      //console.log("argArr", argArr)

      Object.entries(argArr).forEach(([,arg]) => {
        //console.log("arg", arg)
      
        Object.entries(arg).forEach(([key, value]) => {
          //console.log("key", key, "value", value)

          if (key === "desc") {
            if (typeof value === "string") {
              //console.log("this is a string", value)
              value = value.replace("?", "")
            }
            tempArr.push(value)
          }
        })
      })
    }
  })

  let boo = true;
  //console.log("argumentTypes ", argumentTypes)
  argumentTypes.value.forEach((arrType) => {
    //console.log("!tempArr.includes(arrType)", !tempArr.includes(arrType))
    if(!tempArr.includes(arrType)) {
      boo = false
    }
  })
  //console.log("return", boo)

  if(argumentTypes.value.length === 0) {
    boo = false
  }
  return boo
}

function checkTags(foo: [string, string | object]) {
  let boo = false;

  Object.entries(foo).forEach(([name, tagArr]) => {
    if (name === "tags") {

      Object.entries(tagArr).forEach(([, tag]) => {
        if(tags.value.includes(tag as string)) {
          boo = true;
        }
      })
    }
  })
  return boo
}


function checkTagsAnd(foo: [string, string | object]) {
  let boo = true;

  const tempArr: any[] = []

  Object.entries(foo).forEach(([name, tagArr]) => {
    if (name === "tags") {

      Object.entries(tagArr).forEach(([, tag]) => {
        tempArr.push(tag)

      })
    }
  })

  tags.value.forEach((arrTag) => {
    // console.log("tempArr", tempArr)
    // console.log("arrTag", arrTag)
    // console.log("!tempArr.includes(arrTag)", !tempArr.includes(arrTag))
    if(!tempArr.includes(arrTag)) {
      boo = false
    }
  })

  if(tags.value.length === 0) {
    boo = false
  }

  return boo
}


function addToLib() {
  const newArr: any[] = []
  console.log("a", aBool.value)
  console.log("t", tBool.value === "or")
  
  Object.entries(libs).forEach(([, [, lib]]) => {
    Object.entries(lib).forEach(([, foo]) => {
      console.log("                     foo ", foo)
      const checkRet = checkReturn(foo)
      const checkArg = (aBool.value === "or") ? checkArgs(foo) : checkArgsAnd(foo)
      const checkTag = (tBool.value === "or")? checkTags(foo) : checkTagsAnd(foo)
      console.log("checkReturn(foo)", checkRet)
      console.log("(aBool.value === 'or') ? checkArgs(foo) : checkArgsAnd(foo)", checkArg)
      console.log("(tBool.value === 'or')? checkTags(foo) : checkTagsAnd(foo)", checkTag)
      console.log("checkReturn(foo)  ||  (aBool.value === 'or') ? checkArgs(foo) : checkArgsAnd(foo) ||  (tBool.value === 'or')? checkTags(foo) : checkTagsAnd(foo)", checkRet  ||  checkArg ||  checkTag)
      console.log("tBool", tBool)
      if (checkRet  ||  checkArg ||  checkTag  ) {
        console.log("push!!AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n\n\n\n")
        newArr.push(foo)
      }
    })
  });
  console.log("newArr", newArr)

  filteredLibs.value = newArr
}


const tagList = ref([
  { id: 1, val: 'list' },
  { id: 2, val: 'list manipulation' },
  { id: 3, val: 'list creation' },
  { id: 4, val: 'math' },
  { id: 5, val: 'algebra' },
  { id: 6, val: 'comparator' },
  { id: 7, val: 'trigonometry' },
  { id: 8, val: 'function composition' },
  { id: 9, val: 'association list' },
  { id: 10, val: 'type check' },
  { id: 11, val: 'string' },
  { id: 12, val: 'boolean/logic' },
  { id: 13, val: 'char' },
  { id: 14, val: 'regexes' },
  { id: 15, val: 'vectors' },
  { id: 16, val: 'mutation' },
  { id: 17, val: 'predicates' },
  { id: 18, val: 'testing' },
  { id: 19, val: 'formatting' },
  { id: 20, val: 'constants' },
  { id: 21, val: 'other' },
  { id: 22, val: 'interactive' },
  { id: 23, val: 'html' },
  { id: 24, val: 'reactive' },
  { id: 25, val: 'images' },
  { id: 26, val: 'color' },
  { id: 27, val: 'rgb' },
  { id: 28, val: 'hsv' },
  { id: 29, val: 'shapes' },
  { id: 30, val: 'composition/placement' },
  { id: 31, val: 'path' },
  { id: 32, val: 'canvas' },
  { id: 33, val: 'pixel' },
  { id: 34, val: 'music' },
  { id: 35, val: 'duration' },
  { id: 36, val: 'instruments' },
  { id: 37, val: 'note' },
  { id: 38, val: 'modifications' },
  { id: 39, val: 'audio' },
  { id: 40, val: 'sound' },
  { id: 41, val: 'data' },
  { id: 42, val: 'create' },
  { id: 43, val: 'plot' },
  { id: 44, val: 'parse' }
]);

const AisOpen = ref(false)
const RisOpen = ref(false)
const TisOpen = ref(false)

const tags = ref<string[]>([])
const argumentTypes = ref<string[]>([])
const returnTypes = ref<string[]>([])

const tBoolArr = ref([
{ id: 7, val: 'or' },
{ id: 8, val: 'and' }
])
const tBool = ref("or")
const aBoolArr = ref([
{ id: 7, val: 'or' },
{ id: 8, val: 'and' }
])
const aBool = ref("or")
// const rBoolArr = ref([
// { id: 7, val: 'or' },
// { id: 8, val: 'and' }
// ])
// const rBool = ref("or")

const types = ref([
  { id: 301, val: 'number'},
  { id: 302, val: 'string' },
  { id: 303, val: 'boolean' },
  { id: 304, val: 'list' },
  { id: 305, val: 'integer' },
  { id: 306, val: 'procedure' }
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


    <p class="fix-margin">Types:{{ argumentTypes }}</p>
    <text>Arguments </text> 
    <button class="arrow-button" @click="AisOpen = !AisOpen">{{ AisOpen ? "▼" : "▶" }}</button>
    <text>-</text> 
    <select v-model="aBool">
      <option v-for="o in aBoolArr" :key="o.id">
        <p><input type="checkbox" class="indent" :value= "o.val" >
        {{ o.val }}</p>
      </option>
    </select>
    <div v-if="AisOpen" class="dropdown-menu">
      <label v-for="o in types" :key="o.id">
        <p><input v-model="argumentTypes" type="checkbox" class="indent" :value= "o.val" >
        {{ o.val }}</p>
      </label>
    </div>
    
    <p></p>
    <p></p>

    <p class="fix-margin">Types:{{ returnTypes }}</p>
    <text>Return </text>
    <button class="arrow-button" @click="RisOpen = !RisOpen">{{ RisOpen ? "▼" : "▶" }}</button>
    <!-- <text>-</text> 
    <select v-model="rBool">
      <option v-for="o in rBoolArr" :key="o.id">
        <p><input type="checkbox" class="indent" :value= "o.val" >
        {{ o.val }}</p>
      </option>
    </select> -->
    <div v-if="RisOpen" class="dropdown-menu">
      <label v-for="o in types" :key="o.id">
        <p><input v-model="returnTypes" type="checkbox" class="indent" :value= "o.val" >
        {{ o.val }}</p>
      </label>
    </div>
    

  </div>

  <h3><strong>Search by Tag</strong> </h3>
  <div class="index">

    <p class="fix-margin" >Tags:{{ tags }}</p>
    <text>Tags </text>
    <button class="arrow-button" @click="TisOpen = !TisOpen">{{ TisOpen ? "▼" : "▶" }}</button>
      <text>-</text> 
    <select v-model="tBool">
      <option v-for="o in tBoolArr" :key="o.id">
        <div>
          <p><input type="checkbox" class="indent" :value= "o.val" >{{ o.val }}</p>
        </div>
      </option>
    </select>
    <div v-if="TisOpen" class="dropdown-menu">
      <label v-for="o in tagList" :key="o.id">
        <p><input v-model="tags" type="checkbox" class="indent" :value= "o.val" >
        {{ o.val }}</p>
      </label>
    </div>
  
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
  margin-left: 1em;
  font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
  flex-shrink: 0;
  min-height: 0;
  width: 90%;
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
  color: #ffacac;
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

.fix-margin {
  margin-top: 5px;
  margin-bottom: -5px;
}

</style>
