<script setup lang="ts">
import { ref, shallowRef } from 'vue'
import * as L from '../../../lpm'
import { ReactiveImageFile } from '../image'
import { imageToCanvas, loadImage } from '../decode'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'

const props = defineProps<{ value: ReactiveImageFile }>()

// `shallowRef` rather than `ref`: a Scamper `Value` is a deeply recursive
// union, and Vue's reactive unwrapping cannot instantiate it. Nothing here
// mutates the value in place, so shallow is also what is wanted.
const result = shallowRef<L.Value>(null)
const isLoading = ref(false)
const error = ref<string | null>(null)

function onFileChange(event: Event) {
  const input = event.target as HTMLInputElement
  if (input.files === null || input.files.length === 0) {
    result.value = null
    return
  }
  isLoading.value = true
  error.value = null
  // An object URL rather than FileReader's data URL: no base64 copy of the
  // whole image, and the same route image-load takes. Revoked on both paths --
  // the image has finished decoding by the time its load resolves.
  const url = URL.createObjectURL(input.files[0])
  loadImage(url, 'Could not read that file as an image')
    .then((img) => {
      // As in ReactiveFileChooserRenderer: through the run the value carries,
      // because this fires long after the step that made it (#397).
      props.value[L.runField].spawn(
        props.value.callback, [imageToCanvas(img)], (r) => {
          result.value = r
          isLoading.value = false
        },
      )
    })
    // Said out loud rather than leaving "Loading..." on screen forever, which
    // is what a file the browser cannot decode used to do.
    .catch((e: unknown) => {
      error.value = e instanceof Error ? e.message : String(e)
      isLoading.value = false
    })
    .finally(() => { URL.revokeObjectURL(url) })
}
</script>

<template>
  <div>
    <input type="file" accept="image/*" @change="onFileChange" />
    <br />
    <div v-if="isLoading">Loading...</div>
    <div v-else-if="error !== null">{{ error }}</div>
    <div v-else-if="result !== null">
      <ValueRenderer :value="result" />
    </div>
  </div>
</template>
