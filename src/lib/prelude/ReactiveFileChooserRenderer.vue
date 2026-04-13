<script setup lang="ts">
import { ref } from "vue"
import * as L from "../../lpm"
import { ReactiveFileChooser } from "./files"
import ValueRenderer from "../../lpm/renderers/vue/ValueRenderer.vue"

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
        try {
          result.value = L.callScamperFn(
            props.value.callback,
            e.target.result as string,
          )
        } catch (err) {
          result.value = err as Error
        }
      }
      isLoading.value = false
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
