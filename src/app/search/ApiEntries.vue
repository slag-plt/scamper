<script setup lang="ts">
import { ref } from 'vue'
import type { FunctionDoc } from '../../scheme/docstring/docstring'
import { docRegistry } from '../../lib'
import {
  functionDocCategories,
  functionDocName,
  predTypeName,
} from '../../scheme/docstring/render'
import DocEntry from './DocEntry.vue'

const props = defineProps<{
  searchIn: string
}>()

// N.B., same module list/order as src/docs/DocsApp.vue; "runtime" is
// LPM-internal plumbing, not user-facing, so it's deliberately excluded.
const moduleOrder = [
  'prelude', 'image', 'lab', 'music', 'test',
  'audio', 'canvas', 'html', 'reactive', 'data', 'rex',
]

function allDocs(): FunctionDoc[] {
  return moduleOrder.flatMap((name) => [...(docRegistry.get(name)?.values() ?? [])])
}

/** Every documented param's predicate, including the rest param if any. */
function argPredicates(doc: FunctionDoc) {
  return [...doc.params, ...(doc.restParam ? [doc.restParam] : [])].map(
    (p) => p.predicate,
  )
}

let filteredLibs = ref<FunctionDoc[]>([])

function showEverything() {
  filteredLibs.value = allDocs()
}

function findSearch() {
  const s = props.searchIn
  const showLibs = allDocs().filter((doc) => functionDocName(doc) === s)
  showLibs.forEach((func) => {pushRelatives(func, showLibs)})
  noSearchText.value = showLibs.length === 0 && props.searchIn !== ''
  filteredLibs.value = showLibs
}

function checkReturn(doc: FunctionDoc): boolean {
  if (returnTypes.value.length === 0) {
    return true
  }
  return returnTypes.value.includes(predTypeName(doc.signature.predicate))
}

function checkArgs(doc: FunctionDoc): boolean {
  if (argumentTypes.value.length === 0) {
    return true
  }
  return argPredicates(doc).some((pred) =>
    argumentTypes.value.includes(predTypeName(pred)),
  )
}

function checkArgsAnd(doc: FunctionDoc): boolean {
  if (argumentTypes.value.length === 0) {
    return false
  }
  const docTypes = argPredicates(doc).map(predTypeName)
  return argumentTypes.value.every((arrType) => docTypes.includes(arrType))
}

function checkTags(doc: FunctionDoc): boolean {
  if (tags.value.length === 0) {
    return true
  }
  const docTags = functionDocCategories(doc)
  return tags.value.some((tag) => docTags.includes(tag))
}

function checkTagsAnd(doc: FunctionDoc): boolean {
  if (tags.value.length === 0) {
    return false
  }
  const docTags = functionDocCategories(doc)
  return tags.value.every((arrTag) => docTags.includes(arrTag))
}

function addToLib() {
  filteredLibs.value = allDocs().filter((doc) => {
    const checkRet = checkReturn(doc)
    const checkArg = aBool.value === 'or' ? checkArgs(doc) : checkArgsAnd(doc)
    const checkTag = tBool.value === 'or' ? checkTags(doc) : checkTagsAnd(doc)
    return checkRet && checkArg && checkTag
  })
}

function pushRelatives(doc: FunctionDoc, showLibs: FunctionDoc[]) {
  const relatives: string[] = []
  relativeText.value = false

  const docTags = functionDocCategories(doc)
  docTags.forEach((arrTag) => {
    if(!tagListStr.value.includes(arrTag)) {
      relatives.push(arrTag)
    }
  })

  allDocs().forEach((func) => {
    if(relatives.includes(functionDocName(func))) {
      showLibs.push(func)
      relativeText.value = true
    }
  })
}

const tagListStr = ref([
  'char',
  'string',
  'list',
    'association list',
    'list creation',
    'list manipulation',
  'vectors',
  'mutation',
  'constants',
  'function composition',
  
  'math',
    'algebra',
    'trigonometry',
    'comparator',
    'boolean/logic',

  'images',
    'color',
    'pixel',
    'rgb',
    'hsv',
    
    'composition/placement',
    'path',
  'canvas',
    'shapes',

  'music',
    'duration',
    'instruments',
    'note',
    'modifications',
    'audio',
    'sound',

  'data',
    'create',
    'plot',
    'parse',
    
  'typecheck',

  'regexes',

  'predicates',
  'testing',
  'formatting',

  'interactive',
  'html',
  'reactive',

  'other',
])

const indentList = ref([
'association list', 
'list creation', 
'list manipulation', 
'algebra', 
'trigonometry', 'comparator' , 
'boolean/logic',
'color',
'rgb' ,
'hsv' ,
'shapes' ,
'composition/placement' ,
'path' ,
'pixel' ,
'duration',
'instruments' ,
'note' ,
'modifications' ,
'sound' ,
'create' ,
'plot' ,
'parse' ,
])

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
const tBool = ref('or')
const aBoolArr = ref([
{ id: 7, val: 'or' },
{ id: 8, val: 'and' }
])
const aBool = ref('or')
// const rBoolArr = ref([
// { id: 7, val: 'or' },
// { id: 8, val: 'and' }
// ])
// const rBool = ref("or")

const noSearchText = ref(filteredLibs.value.length === 0 && props.searchIn !== '' && !((argumentTypes.value.length !== 0 || returnTypes.value.length !== 0 || tags.value.length !== 0)))
const noTagText = ref(filteredLibs.value.length === 0 && props.searchIn !== '' && ((argumentTypes.value.length !== 0 || returnTypes.value.length !== 0 || tags.value.length !== 0)))
const relativeText = ref(false)

const types = ref([
  { id: 307, val: 'any' },
  { id: 324, val: 'audio-node' },
  { id: 325, val: 'audio' },
  { id: 303, val: 'boolean' },
  { id: 314, val: 'canvas' },
  { id: 308, val: 'char' },
  { id: 319, val: 'color' },
  { id: 321, val: 'composition' },
  { id: 323, val: 'context' },
  { id: 312, val: 'drawing' },
  { id: 322, val: 'duration' },
  { id: 326, val: 'element' },
  { id: 318, val: 'font' },
  { id: 317, val: 'function' },
  { id: 313, val: 'hsv' },
  { id: 305, val: 'integer' },
  { id: 304, val: 'list' },
  { id: 320, val: 'mod' },
  // { id: 310, val: 'none' },
  { id: 301, val: 'number'},
  { id: 306, val: 'procedure' },
  { id: 309, val: 'ref' },
  { id: 328, val: 'rex' },
  { id: 315, val: 'rgb' },
  { id: 316, val: 'rgb-component' },
  { id: 302, val: 'string' },
  { id: 327, val: 'text-area' },
  { id: 311, val: 'vector' },
])

</script>

<!-- Tag selection goes here -->
<template>

  <div class="flex-box">
    <div>
    <h3 :style="{ fontFamily: 'helvetica'}"> <strong>Filter by Type</strong> </h3>
    <div class="index">


    <p class="fix-margin">Types:{{ argumentTypes }}</p>
    <text>Arguments </text> 
    <button class="arrow-button" @click="AisOpen = !AisOpen">{{ AisOpen ? "▼" : "▶" }}</button>
    <text>-</text> 
    <select v-model="aBool">
      <option v-for="o in aBoolArr" :key="o.id" :value="o.val">{{ o.val }}</option>
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

  <h3 :style="{ fontFamily: 'helvetica'}"><strong>Filter by Tag</strong> </h3>
  <div class="index">

    <p class="fix-margin" >Tags:{{ tags }}</p>
    <text>Tags </text>
    <button class="arrow-button" @click="TisOpen = !TisOpen">{{ TisOpen ? "▼" : "▶" }}</button>
      <text>-</text> 
    <select v-model="tBool">
      <option v-for="o in tBoolArr" :key="o.id" :value="o.val">{{ o.val }}</option>
    </select>
    <div v-if="TisOpen" class="dropdown-menu">
      <label v-for="o in tagListStr" :key="o">
        <div class="flex-box-skinny">
          <div v-if="indentList.includes(o)" class="left-margin"></div>
          <p><input v-model="tags" type="checkbox" class="indent" :value= "o" >
          {{ o }}</p>
        </div>
      </label>
    </div>
  
    </div>
      <button
        class="enter-button" @click="() => { 
          addToLib(); 
          noSearchText = filteredLibs.length === 0 && props.searchIn !== '' && !((argumentTypes.length !== 0 || returnTypes.length !== 0 || tags.length !== 0));
          noTagText = filteredLibs.length === 0 && ((argumentTypes.length !== 0 || returnTypes.length !== 0 || tags.length !== 0));
          //console.log('noSearchText', noSearchText, 'tag', noTagText, 'filteredLibs.length', filteredLibs.length, 'argumentTypes.length', argumentTypes.length, 'returnTypes.length', returnTypes.length, 'tags.length', tags.length, 'boolean', filteredLibs.length === 0 && ((argumentTypes.length !== 0 || returnTypes.length !== 0 || tags.length !== 0)));
        }"><strong>Enter</strong></button>
    </div>

    <div>
      <h3 :style="{ fontFamily: 'helvetica'}"> <strong>Search results</strong> </h3>
      <div class="api">
        <div class="index2"> 
          <div class="entries"> 
            {{(filteredLibs.length === 0)? findSearch() : null }} 
            <div v-if="noSearchText"> {{"No search results found for " + props.searchIn}} </div>
            <div v-if="noTagText"> {{"No filter results found"}} </div>
            <!-- {{(filteredLibs.length === 0 && props.searchIn !== "" && !((argumentTypes.length !== 0 || returnTypes.length !== 0 || tags.length !== 0)))? "No search results found for " + props.searchIn : null}}  -->
            <!-- {{(filteredLibs.length !== 0 && (argumentTypes.length !== 0 || returnTypes.length !== 0 || tags.length !== 0))? "No tag filter results found" : null}}  -->
            {{(filteredLibs.length === 0)? showEverything() : null}}
              <div v-for="(foo, index) in filteredLibs" :key="functionDocName(foo)" ref="foo['name']">
                <h3 v-if="relativeText" :style="{ fontFamily: 'helvetica'}"> {{ (index === 1)? "Related functions" : null }} </h3>
                <DocEntry
                  :id="functionDocName(foo)"
                  :key="functionDocName(foo)"
                  :doc="foo"
                />
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
  width: 74vw;
  margin: -1em
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
  color: #4BADAC;
}

.flex-box {
  display: flex;
  flex-direction: row;
  gap: 70px;
}

.flex-box-skinny {
  display: flex;
  flex-direction: row;
  margin-bottom: -15px;
  margin-top: 0px;
}

.left-margin{
  margin-left: 20px
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
