<script setup lang="ts">
import { ref, shallowRef } from 'vue'
import * as L from '../../../lpm'
import { ReactiveImageFile } from '../image'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'

const props = defineProps<{ value: ReactiveImageFile }>()

// `shallowRef` rather than `ref`: a Scamper `Value` is a deeply recursive
// union, and Vue's reactive unwrapping cannot instantiate it. Nothing here
// mutates the value in place, so shallow is also what is wanted.
const result = shallowRef<L.Value>(null)
const isLoading = ref(false)

function onFileChange(event: Event) {
  const input = event.target as HTMLInputElement
  if (input.files !== null && input.files.length > 0) {
    isLoading.value = true
    const reader = new FileReader()
    reader.onload = (e) => {
      if (e.target !== null) {
        const img = new Image()
        img.onload = () => {
          const canvas = document.createElement('canvas')
          const ctx = canvas.getContext('2d')
          if (ctx) {
            canvas.width = img.width
            canvas.height = img.height
            ctx.drawImage(img, 0, 0)
          }
          // As in ReactiveFileChooserRenderer: through the run the value
          // carries, because this fires long after the step that made it
          // (#397).
          props.value[L.runField].spawn(props.value.callback, [canvas], (r) => {
            result.value = r
            isLoading.value = false
          })
        }
        img.src = e.target.result as string
      }
    }
    reader.readAsDataURL(input.files[0])
  } else {
    result.value = null
  }
}
</script>

<template>
  <div>
    <input type="file" accept="image/*" @change="onFileChange" />
    <br />
    <div v-if="isLoading">Loading...</div>
    <div v-else-if="result !== null">
      <ValueRenderer :value="result" />
    </div>
  </div>
</template>
