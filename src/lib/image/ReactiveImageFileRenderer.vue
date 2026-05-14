<script setup lang="ts">
import { ref } from "vue"
import * as L from "../../lpm"
import { ReactiveImageFile } from "./image"
import ValueRenderer from "../../lpm/renderers/vue/ValueRenderer.vue"

const props = defineProps<{ value: ReactiveImageFile }>()

const result = ref<any>(null)
const isLoading = ref(false)

function onFileChange(event: Event) {
  const input = event.target as HTMLInputElement
  if (input.files !== null && input.files.length > 0) {
    isLoading.value = true
    const reader = new FileReader()
    reader.onload = (e) => {
      if (e !== null && e.target !== null) {
        const img = new Image()
        img.onload = () => {
          const canvas = document.createElement("canvas")
          const ctx = canvas.getContext("2d")
          if (ctx) {
            canvas.width = img.width
            canvas.height = img.height
            ctx.drawImage(img, 0, 0)
          }
          try {
            result.value = L.callScamperFn(props.value.callback, canvas)
          } catch (err) {
            result.value = err as Error
          }
          isLoading.value = false
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
