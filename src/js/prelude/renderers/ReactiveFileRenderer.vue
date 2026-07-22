<script setup lang="ts">
import { ref, onMounted, watch } from 'vue'
import * as L from '../../../lpm'
import { getFS } from '../../../fs'
import { ReactiveFile } from '../files'
import ValueRenderer from '../../../lpm/renderers/vue/ValueRenderer.vue'

const props = defineProps<{ value: ReactiveFile }>()

const result = ref<any>(null)
const isLoading = ref(true)
const opfsSupported = ref(true)

async function loadFile() {
  isLoading.value = true
  try {
    const exists = await getFS().fileExists(props.value.filename)
    if (!exists) {
      result.value = new L.ScamperError('Runtime', `File not found: ${props.value.filename}`)
    } else {
      const data = await getFS().loadFile(props.value.filename)
      try {
        result.value = L.callScamperFn(props.value.callback, data)
      } catch (e) {
        result.value = e as Error
      }
    }
  } catch (e) {
    result.value = e as Error
  } finally {
    isLoading.value = false
  }
}

onMounted(loadFile)
watch(() => props.value, loadFile)
</script>

<template>
  <div>
    <div v-if="!opfsSupported">OPFS not supported</div>
    <div v-else-if="isLoading">Loading...</div>
    <div v-else-if="result !== null">
      <template v-if="result instanceof L.SubthreadErrors">
        <ValueRenderer v-for="(err, index) in result.errors" :key="index" :value="err" />
      </template>
      <ValueRenderer v-else :value="result" />
    </div>
  </div>
</template>
