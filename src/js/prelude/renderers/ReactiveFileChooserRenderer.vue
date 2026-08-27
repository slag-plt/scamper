<script setup lang="ts">
import { ref } from 'vue'
import * as L from '../../../lpm'
import { ReactiveFileChooser } from '../files'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'

const props = defineProps<{ value: ReactiveFileChooser }>()

const result = ref<any>(null)
const isLoading = ref(false)

function onFileChange(event: Event) {
  const input = event.target as HTMLInputElement
  if (input.files !== null && input.files.length > 0) {
    isLoading.value = true
    const reader = new FileReader()
    reader.onload = (e) => {
      if (e !== null && e.target !== null) {
        // Run the callback as a fiber (JS can no longer call the closure) and
        // render its result in the widget; a callback error surfaces in the
        // output pane instead. Through the run the *value* carries, since this
        // fires long after the step that made it and Vue mounts us later still
        // -- resolving a run here would find the foreground one, or none at
        // all on a reading page (#397).
        props.value[L.runField].spawn(props.value.callback, [e.target.result as string], (r) => {
          result.value = r
          isLoading.value = false
        })
      } else {
        isLoading.value = false
      }
    }
    reader.readAsText(input.files[0])
  } else {
    result.value = null
  }
}
</script>

<template>
  <div>
    <input type="file" @change="onFileChange" />
    <br />
    <div v-if="isLoading">Loading...</div>
    <div v-else-if="result !== null">
      <ValueRenderer :value="result" />
    </div>
  </div>
</template>
