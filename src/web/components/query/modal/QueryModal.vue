<script setup lang="ts">
import { ModalCols, ModalPadding } from "../query-sizing"
import { QueryEntry } from "../../../../scamper"
import { computed, useTemplateRef } from "vue"
import { SimpleErrorChannel } from "../../../../lpm/output/simple-error"
import { ReportError } from "../../../../lpm"
import ModalContents from "./ModalContents.vue"
import ModalControls from "./ModalControls.vue"

const { query } = defineProps<{ query: QueryEntry }>()

const toRender = computed(() => {
  if (!(query.err instanceof SimpleErrorChannel)) {
    return "Fatal query error"
  }
  const firstErr = query.err.errors.at(0)
  // TODO: don't forget done wiring
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

const contentsRef = useTemplateRef("contents-ref")
</script>

<template>
  <div id="query-modal" :data-query-id="query.id">
    <ModalContents ref="contents-ref" :value="toRender" />
    <ModalControls
      :id="query.id"
      :overflowing="contentsRef?.isOverflowing ?? false"
    />
  </div>
</template>

<style scoped>
#query-modal {
  display: flex;
  justify-content: space-between;
  box-sizing: border-box;
  box-shadow: 0 0 v-bind("paddingVertical") rgba(0, 0, 0, 0.3);
  background-color: white;
  width: v-bind("width");
  min-width: 0;
  padding: v-bind("padding");
  overflow: clip;
  overflow-clip-margin: content-box;
}
</style>
