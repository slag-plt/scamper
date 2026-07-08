<script setup lang="ts">
import ValueRenderer from "../../../../lpm/renderers/vue/ValueRenderer.vue"
import { Value } from "../../../../lpm"

defineProps<{ value: Value }>()
</script>

<template>
  <div id="query-contents">
    <!--  TODO: do smarter rendering of non-Scamper values  -->
    <ValueRenderer :value="value" />
  </div>
</template>

<style scoped>
#query-contents {
  white-space: normal;
  overflow-wrap: break-word;
  /* TODO: such a dumb hack for line clamping... wait for line-clamp to be real */
  display: -webkit-box;
  -webkit-box-orient: vertical;
  -webkit-line-clamp: 2;
  flex: 1;
  min-width: 0;
}
</style>
