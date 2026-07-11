<script setup lang="ts">
import { shallowRef, watchEffect } from "vue"
import type { RecursivePage } from "../../../lpm/reporting/pruning"
import { useScamperSession } from "../../composables/use-scamper-session"
import { renderRecursivePage, type PageSourceSegment } from "./page-render"

const { id, page } = defineProps<{ id: number; page: RecursivePage }>()

const { closePageModal, openLinkedPageModal } = useScamperSession()
const segments = shallowRef<PageSourceSegment[]>([])
const formattingError = shallowRef<string | null>(null)

watchEffect((onCleanup) => {
  let active = true
  onCleanup(() => {
    active = false
  })
  segments.value = []
  formattingError.value = null
  void renderRecursivePage(page)
    .then((rendered) => {
      if (active) {
        segments.value = rendered
      }
    })
    .catch((error: unknown) => {
      if (active) {
        formattingError.value =
          error instanceof Error
            ? error.message
            : "Could not format recursive page"
      }
    })
})
</script>

<template>
  <div class="page-modal">
    <div class="page-modal-contents">
      <pre
        v-if="segments.length > 0"
        class="page-source"
      ><template v-for="(segment, index) in segments" :key="index"><button
        v-if="segment.tag === 'link'"
        class="page-link"
        :class="{ skipped: segment.link.skippedInvocations.length > 0 }"
        @click="openLinkedPageModal(segment.link.targetPage)"
      >{{ segment.text }}</button><template v-else>{{ segment.text }}</template></template></pre>
      <div v-else-if="formattingError">{{ formattingError }}</div>
      <div v-else>Formatting…</div>
    </div>
    <div class="page-modal-controls">
      <button @click.stop="closePageModal(id)">X</button>
    </div>
  </div>
</template>

<style scoped>
.page-modal {
  position: fixed;
  z-index: 2;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  width: fit-content;
  height: fit-content;
  background: white;
  padding: 1rem;
  box-shadow: 0 0 0.5rem rgba(0, 0, 0, 0.3);
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  gap: 1rem;
}

.page-modal-contents {
  min-width: 0;
}

.page-modal-controls {
  display: flex;
  flex-direction: column;
}

.page-source {
  margin: 0;
}

.page-link {
  font: inherit;
  white-space: pre;
}
</style>
