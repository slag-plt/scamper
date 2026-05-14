<script setup lang="ts">
defineProps<{
  libs: [string, object][]
  selectedModule: string
}>()

const emit = defineEmits<{
  select: [name: string]
}>()
</script>

<template>
  <div class="modules">
    <ul>
      <template v-for="([name], i) in libs" :key="name">
        <li>
          <button
            :class="{ active: name === selectedModule }"
            @click="emit('select', name)"
          >{{ name }}</button>
        </li>
        <li v-if="i < libs.length - 1" aria-hidden="true">⋅</li>
      </template>
    </ul>
  </div>
</template>

<style scoped>
.modules {
  padding: 0;
  margin: 0;
  text-align: center;
}

ul {
  list-style: none;
  margin: 1em 0;
  padding: 0;
  display: inline-flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0;
}

li {
  display: inline;
}

li[aria-hidden] {
  padding: 0 0.25em;
}

button {
  margin: 0;
  padding: 0;
  background-color: transparent;
  border: none;
  cursor: pointer;
}

button:hover {
  text-decoration: underline;
}

button.active {
  font-weight: bold;
}
</style>
