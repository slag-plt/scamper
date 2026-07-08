<script setup lang="ts">
import { onBeforeUnmount, onMounted, useTemplateRef } from "vue"
import { useScamperSession } from "../../composables/use-scamper-session"

const popover = useTemplateRef<HTMLDivElement>("modal-ref")

const { collapseQuery } = useScamperSession()

const handleClick = (e: MouseEvent) => {
  const el = popover.value
  if (!el) {
    return
  }
  if (el.contains(e.target as Node)) {
    return
  }
  // close popover
  console.log("closing")
  collapseQuery()
}

onMounted(() => {
  document.addEventListener("click", handleClick)
})
onBeforeUnmount(() => {
  document.removeEventListener("click", handleClick)
})
</script>

<template>
  <Teleport to="body">
    <div id="expanded-query-modal" ref="modal-ref">hi</div>
  </Teleport>
</template>

<style scoped>
#expanded-query-modal {
  position: fixed;
  z-index: 1;
  top: 0;
  left: 0;
  height: fit-content;
  width: fit-content;
}
</style>
