<script setup lang="ts">
import {
  getQueryAnchorName,
  ModalOverallPadding,
  ModalVerticalPadding,
  ModalWidth,
  useReportedValue,
} from '../query-utils'
import { QueryEntry } from '../../../../scamper'
import { computed, useTemplateRef } from 'vue'
import ModalContents from './ModalContents.vue'
import ModalControls from './ModalControls.vue'
import { useScamperSession } from '../../../composables/use-scamper-session'

const { query } = defineProps<{ query: QueryEntry }>()

const { expandedQueryId } = useScamperSession()

const toRender = useReportedValue(() => query)
const queryAnchorName = computed(() => getQueryAnchorName(query.id))

void ModalWidth
void ModalVerticalPadding
void ModalOverallPadding

const invisible = computed(() => {
  const expanded = expandedQueryId.value
  if (expanded === null) {
    return false
  }
  return query.id === expanded
})

const contentsRef = useTemplateRef('contents-ref')
</script>

<template>
  <div class="query-modal" :data-query-id="query.id" :class="{ invisible }">
    <ModalContents ref="contents-ref" :value="toRender" />
    <ModalControls
      :query-id="query.id"
      :overflowing="contentsRef?.isOverflowing ?? false"
    />
  </div>
</template>

<style scoped>
.query-modal {
  display: flex;
  justify-content: space-between;
  box-sizing: border-box;
  box-shadow: 0 0 v-bind("ModalVerticalPadding") rgba(0, 0, 0, 0.3);
  background-color: white;
  width: v-bind("ModalWidth");
  min-width: 0;
  padding: v-bind("ModalOverallPadding");
  overflow: clip;
  overflow-clip-margin: content-box;
}

.invisible {
  opacity: 0;
  pointer-events: none;
}

@supports (anchor-name: --test) {
  .query-modal {
    anchor-name: v-bind("queryAnchorName");
  }
}
</style>
