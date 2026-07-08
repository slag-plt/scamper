<script setup lang="ts">
import { ModalCols, ModalPadding } from "./query-sizing"
import { QueryEntry } from "../../../scamper"
import { computed } from "vue"
import { SimpleErrorChannel } from "../../../lpm/output/simple-error"
import { ReportError } from "../../../lpm"
import ValueRenderer from "../../../lpm/renderers/vue/ValueRenderer.vue"

const { query } = defineProps<{ query: QueryEntry }>()

const toRender = computed(() => {
  if (!(query.err instanceof SimpleErrorChannel)) {
    return "Fatal query error"
  }
  const firstErr = query.err.errors.at(0)
  if (!firstErr) {
    return "No query found."
  }
  if (!(firstErr instanceof ReportError)) {
    return firstErr
  }
  return firstErr.value
})

const width = `${ModalCols.toString()}ch`
const paddingVertical = `${ModalPadding.toString()}lh`
const paddingHorizontal = `${ModalPadding.toString()}ch`
const padding = `${paddingVertical} ${paddingHorizontal}`
</script>

<template>
  <div id="query-modal">
    <ValueRenderer :value="toRender" />
  </div>
</template>

<style scoped>
#query-modal {
  box-sizing: border-box;
  background-color: lightblue;
  width: v-bind("width");
  padding: v-bind("padding");
  white-space: normal;
  line-clamp: 2;
  overflow-wrap: break-word;
  overflow: clip;
  overflow-clip-margin: content-box;
  /* TODO: such a dumb hack for line clamping... wait for line-clamp to be real */
  display: -webkit-box;
  -webkit-box-orient: vertical;
  -webkit-line-clamp: 2;
  min-width: 0;
}
</style>
