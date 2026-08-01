<script setup lang="ts">
import { computed, onMounted, onUnmounted } from 'vue'

/** One entry of the context menu: an action, or a separator. */
export interface MenuItem {
  label?: string
  /** Shortcut hint shown on the right, e.g. "Alt+.". */
  kbd?: string
  run?: () => void | Promise<void>
  disabled?: boolean
  separator?: boolean
}

const props = defineProps<{ x: number; y: number; items: MenuItem[] }>()
const emit = defineEmits<{ close: [] }>()

// Keep the menu inside the viewport (rough clamp; the panel is ~210px wide).
const style = computed(() => ({
  left: `${String(Math.min(props.x, window.innerWidth - 220))}px`,
  top: `${String(Math.min(props.y, window.innerHeight - 24 * props.items.length - 16))}px`,
}))

function choose(item: MenuItem) {
  if (item.disabled || item.separator || item.run === undefined) {
    return
  }
  void item.run()
  emit('close')
}

function onDocMouseDown() {
  emit('close')
}

function onKeyDown(e: KeyboardEvent) {
  if (e.key === 'Escape') {
    emit('close')
  }
}

onMounted(() => {
  // Defer so the opening right-click's own mouseup/down doesn't immediately close it.
  setTimeout(() => {
    document.addEventListener('mousedown', onDocMouseDown)
  }, 0)
  document.addEventListener('keydown', onKeyDown)
})
onUnmounted(() => {
  document.removeEventListener('mousedown', onDocMouseDown)
  document.removeEventListener('keydown', onKeyDown)
})
</script>

<template>
  <ul class="context-menu" :style="style" role="menu" @mousedown.stop>
    <template v-for="(item, i) in items" :key="i">
      <li v-if="item.separator" class="separator" role="separator"></li>
      <li
        v-else
        role="menuitem"
        :class="{ disabled: item.disabled }"
        :aria-disabled="item.disabled"
        @click="choose(item)"
      >
        <span class="label">{{ item.label }}</span>
        <span v-if="item.kbd" class="kbd">{{ item.kbd }}</span>
      </li>
    </template>
  </ul>
</template>

<style scoped>
.context-menu {
  position: fixed;
  z-index: 20;
  min-width: 200px;
  margin: 0;
  padding: 0.25em 0;
  list-style: none;
  background: var(--surface);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: 6px;
  box-shadow: 0 6px 20px rgba(0, 0, 0, 0.25);
  font-size: 0.85rem;
  user-select: none;
}

li[role='menuitem'] {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 1.5em;
  padding: 0.3em 0.85em;
  cursor: pointer;
}

li[role='menuitem']:hover:not(.disabled) {
  background: var(--surface-hover);
}

li.disabled {
  opacity: 0.4;
  cursor: default;
}

.kbd {
  font-size: 0.75rem;
  opacity: 0.6;
}

.separator {
  height: 1px;
  margin: 0.25em 0;
  background: var(--border);
}
</style>
