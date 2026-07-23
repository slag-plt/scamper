<script setup lang="ts">
import ValueRenderer from '../../../../../lpm/renderers/vue/ValueRenderer.vue'
import { Value } from '../../../../../lpm'
import { nextTick, onMounted, ref, useTemplateRef, watch } from 'vue'

const props = withDefaults(defineProps<{ value: Value; clip?: boolean }>(), {
  clip: true,
})

const divRef = useTemplateRef('div-ref')
const isOverflowing = ref(false)

const checkOverflow = () => {
  const el = divRef.value
  if (!el) {
    return
  }
  isOverflowing.value =
    el.scrollHeight > el.clientHeight || el.scrollWidth > el.clientWidth
}

onMounted(() => {
  checkOverflow()
})

watch(
  () => props.value,
  async () => {
    await nextTick()
    checkOverflow()
  },
)

defineExpose({ isOverflowing })
</script>

<template>
  <div
    id="query-contents"
    ref="div-ref"
    :class="{
      'overflow-gradient': clip && isOverflowing,
      clip,
    }"
  >
    <!--  TODO: do smarter rendering of non-Scamper values  -->
    <ValueRenderer :value="value" />
  </div>
</template>

<style scoped>
#query-contents {
  flex: 1;
  min-width: 0;
}

.clip {
  white-space: normal;
  overflow-wrap: break-word;
  /* TODO: such a dumb hack for line clamping... wait for line-clamp to be real */
  display: -webkit-box;
  -webkit-box-orient: vertical;
  -webkit-line-clamp: 2;
  overflow: hidden;
}

.overflow-gradient {
  mask-image:
    linear-gradient(to bottom, black 75%, transparent 100%),
    linear-gradient(to right, black 75%, transparent 100%);
  mask-composite: intersect;
}
</style>
