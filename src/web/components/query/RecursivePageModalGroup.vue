<script setup lang="ts">
import { onBeforeUnmount, onMounted, useTemplateRef } from "vue"
import { useScamperSession } from "../../composables/use-scamper-session"
import PageModal from "./PageModal.vue"

const group = useTemplateRef<HTMLDivElement>("page-modal-group")
const { collapsePageModals, pageModalStack } = useScamperSession()

function handleDocumentClick(event: MouseEvent) {
  if (!group.value?.contains(event.target as Node)) {
    collapsePageModals()
  }
}

onMounted(() => {
  document.addEventListener("click", handleDocumentClick)
})
onBeforeUnmount(() => {
  document.removeEventListener("click", handleDocumentClick)
})
</script>

<template>
  <Teleport to=".cm-editor">
    <div ref="page-modal-group">
      <PageModal
        v-for="entry in pageModalStack"
        :id="entry.id"
        :key="entry.id"
        :page="entry.page"
      />
    </div>
  </Teleport>
</template>
