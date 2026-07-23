<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, useTemplateRef } from 'vue'
import { useScamperSession } from '../../composables/use-scamper-session'
import ModalContents from './modal/ModalContents.vue'
import {
  getQueryAnchorName,
  ModalOverallPadding,
  ModalVerticalPadding,
  ModalWidth,
  useReportedValue,
} from './query-utils'
import ModalControls from './modal/ModalControls.vue'
import { SchedulerId } from '../../../../lpm/scheduler'

const { queryId } = defineProps<{ queryId: SchedulerId }>()

const popover = useTemplateRef<HTMLDivElement>('modal-ref')

const { collapseQuery, getQueryOrThrow } = useScamperSession()

const queryAnchorName = computed(() => getQueryAnchorName(queryId))

const handleClick = (e: MouseEvent) => {
  const el = popover.value
  if (!el) {
    return
  }
  if (el.contains(e.target as Node)) {
    return
  }
  // close popover
  console.log('closing')
  collapseQuery()
}

const toRender = useReportedValue(() => getQueryOrThrow(queryId))

void ModalWidth
void ModalVerticalPadding
void ModalOverallPadding

onMounted(() => {
  document.addEventListener('click', handleClick)
})
onBeforeUnmount(() => {
  document.removeEventListener('click', handleClick)
})
</script>

<template>
  <Teleport to=".cm-editor">
    <div id="expanded-query-modal" ref="modal-ref">
      <ModalContents :value="toRender" :clip="false" />
      <ModalControls :query-id="queryId" />
    </div>
  </Teleport>
</template>

<style scoped>
#expanded-query-modal {
  position: fixed;
  z-index: 1;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  height: fit-content;
  width: fit-content;
  background: gray;

  display: flex;
  justify-content: space-between;
  box-sizing: border-box;
  box-shadow: 0 0 v-bind("ModalVerticalPadding") rgba(0, 0, 0, 0.3);
  background-color: white;
  min-width: 0;
  padding: v-bind("ModalOverallPadding");
}

@supports (anchor-name: --test) {
  #expanded-query-modal {
    position-anchor: v-bind("queryAnchorName");
    position-area: top center;
    position-visibility: anchors-visible;
    top: auto;
    left: anchor(center);
    bottom: anchor(bottom);
    transform: translate(-50%, -0.5rem);
  }
}
</style>
