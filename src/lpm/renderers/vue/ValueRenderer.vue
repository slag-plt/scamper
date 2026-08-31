<script setup lang="ts">
import { Value } from '../../index'
import { computed, PropType } from 'vue'
import VueRenderer from '../vue'

// Declared at runtime, not as `defineProps<{ value: Value }>()`: `Value` is a
// union including `boolean`, so the compiler would put `Boolean` in the prop's
// runtime type, and Vue casts an incoming `''` -- or the prop's own name -- to
// `true` for a Boolean prop. Every empty string then printed as `#t` (#444).
// `type: null` accepts anything and casts nothing, which is why
// createTextRenderer (vue.ts) declares its own `value` the same way.
const props = defineProps({
  value: { type: null as unknown as PropType<Value>, required: true },
})

const resolvedComponent = computed(() => {
  return VueRenderer.render(props.value)
})
</script>

<template>
  <component :is="resolvedComponent" :value="value" />
</template>

<style scoped></style>
