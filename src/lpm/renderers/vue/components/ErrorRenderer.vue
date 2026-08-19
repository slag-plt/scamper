<script setup lang="ts">
import { computed } from 'vue'

/**
 * An error, rendered as bold italic text rather than as the plain monospace
 * every other value gets. An error is the one thing in the output a person
 * needs to spot without reading, and the weight is what does that.
 *
 * It takes the error itself rather than a formatted string, so a later change
 * can lay out its parts -- the phase, the `[line:col]` range, the message --
 * instead of the single line `toString` flattens them into.
 */
const props = defineProps<{ value: Error }>()

const text = computed(() => props.value.toString())
</script>

<template>
  <strong class="error-text" tabindex="0">{{ text }}</strong>
</template>

<style scoped>
/* --danger rather than --error-accent: the latter's light value is a pale
   background tint (#ffacac), which as text lands around 1.8:1 on white. */
.error-text {
  color: var(--danger);
  font-style: italic;
  font-weight: 700;
}
</style>
