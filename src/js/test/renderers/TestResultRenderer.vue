<script setup lang="ts">
import { structKind } from '../../../lpm/lang'
import { Result } from '../index'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'

defineProps<{ value: Result }>()
</script>

<template>
  <div v-if="value[structKind] === 'ok'" class="test-result ok">
    Test "{{ value.desc }}": Passed! ✅
  </div>
  <div v-else-if="value[structKind] === 'exp'" class="test-result error">
    Test "{{ value.desc }}": Failed! ❌
    <hr />
    <span>
      Expected <ValueRenderer :value="value.expected" />, received <ValueRenderer :value="value.actual" />
    </span>
  </div>
  <div v-else-if="value[structKind] === 'exn'" class="test-result error">
    Test "{{ value.desc }}": Failed! ❌
    <hr />
    <span>
      Test case threw an exception: <ValueRenderer :value="value.exn" />
    </span>
  </div>
  <div v-else-if="value[structKind] === 'gen'" class="test-result error">
    Test "{{ value.desc }}": Failed! ❌
    <hr />
    {{ value.reason }}
  </div>
</template>

<style scoped>
.test-result {
  font-family:
    Menlo,
    Consolas,
    Monaco,
    Liberation Mono,
    Lucida Console,
    monospace;
  font-size: 1em;
  margin: 0.5em;
  padding: 0.25em;
  border: dashed 1px var(--test-border);
}

.test-result.ok {
  background-color: var(--test-ok-bg);
}

.test-result.error {
  background-color: var(--test-error-bg);
}
</style>
