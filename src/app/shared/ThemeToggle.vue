<script setup lang="ts">
import { computed } from 'vue'
import { currentTheme, toggleTheme } from '../../theme'

const isDark = computed(() => currentTheme.value === 'dark')
const label = computed(() =>
  isDark.value ? 'Switch to light theme' : 'Switch to dark theme',
)
</script>

<template>
  <button
    class="fa-solid"
    :class="isDark ? 'fa-sun' : 'fa-moon'"
    :aria-label="label"
    :title="label"
    @click="toggleTheme()"
  ></button>
</template>
