<script setup lang="ts">
import { computed, nextTick, onMounted, onUnmounted, ref, useId, watch } from 'vue'
import type { MenuItem } from '../menu'

/**
 * A floating menu pinned at a point on the screen: the editor's right-click
 * menu, the file drawer's per-file menu, and each menu of the menu bar. Closes
 * on Escape or on a click anywhere outside it, and can be walked with the arrow
 * keys.
 */
const props = withDefaults(
  defineProps<{
    x: number
    y: number
    items: MenuItem[]
    /**
     * Starts with the first item highlighted, for a menu opened from the
     * keyboard -- where landing on nothing would mean a second Down just to
     * begin. A menu opened by pointer starts with nothing highlighted.
     */
    autoActivate?: boolean
  }>(),
  { autoActivate: false },
)
const emit = defineEmits<{ close: [] }>()

const menuId = useId()

const root = ref<HTMLUListElement | null>(null)

/**
 * Where the menu sits.
 *
 * Seeded from the click and corrected once the menu has been measured. The
 * guess used to be the whole of it, against a hardcoded 220px width and 24px
 * rows -- neither of which the CSS guarantees -- so a long File menu in a short
 * window was given a negative `top` and lost its first items off the screen.
 */
const MARGIN = 8
const pos = ref({ left: props.x, top: props.y })

const style = computed(() => ({
  left: `${String(pos.value.left)}px`,
  top: `${String(pos.value.top)}px`,
}))

function reposition() {
  const el = root.value
  if (el === null) return
  const { width, height } = el.getBoundingClientRect()
  pos.value = {
    left: Math.max(MARGIN, Math.min(props.x, window.innerWidth - width - MARGIN)),
    top: Math.max(MARGIN, Math.min(props.y, window.innerHeight - height - MARGIN)),
  }
}

const hasChecks = computed(() =>
  props.items.some((item) => item.checked !== undefined),
)

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

// ---------- keyboard navigation ----------

/** The highlighted item's index, or -1 before the arrow keys are used. */
const activeIndex = ref(-1)

function isSelectable(item: MenuItem): boolean {
  return item.separator !== true && item.disabled !== true
}

/**
 * The next selectable item from `start`, stepping by `step` and wrapping.
 * Separators and greyed-out entries are skipped rather than landed on.
 * @returns the index, or -1 when nothing in the menu can be selected.
 */
function nextSelectable(start: number, step: number): number {
  const n = props.items.length
  for (let i = 1; i <= n; i++) {
    const idx = (((start + step * i) % n) + n) % n
    if (isSelectable(props.items[idx])) return idx
  }
  return -1
}

function onKeyDown(e: KeyboardEvent) {
  switch (e.key) {
    case 'Escape':
      emit('close')
      return
    case 'ArrowDown':
    case 'ArrowUp': {
      e.preventDefault()
      const step = e.key === 'ArrowDown' ? 1 : -1
      // From nothing highlighted, Down lands on the first item and Up the last.
      activeIndex.value = nextSelectable(
        activeIndex.value === -1 ? (step === 1 ? -1 : 0) : activeIndex.value,
        step,
      )
      return
    }
    case 'Home':
      e.preventDefault()
      activeIndex.value = nextSelectable(-1, 1)
      return
    case 'End':
      e.preventDefault()
      activeIndex.value = nextSelectable(0, -1)
      return
    case 'Enter':
    case ' ':
      if (activeIndex.value >= 0) {
        e.preventDefault()
        choose(props.items[activeIndex.value])
      }
      return
    default:
      return
  }
}

/**
 * Re-place and re-seed whenever the menu is asked to show something else.
 *
 * IdeMenuBar keeps one PopupMenu instance and swaps its props as you slide
 * along the bar, so "the menu changed" is not the same event as "the menu
 * mounted". Without this the panel stays under the title it first opened
 * beneath while showing another one's items, and -- worse -- activeIndex
 * survives into a shorter menu, where Enter reads past the end of the array
 * and throws.
 */
watch(
  () => [props.x, props.y, props.items] as const,
  () => {
    activeIndex.value = props.autoActivate ? nextSelectable(-1, 1) : -1
    void nextTick(reposition)
  },
)

/** Whatever had focus when the menu opened, to hand it back to on close. */
let opener: HTMLElement | null = null

onMounted(() => {
  opener = document.activeElement as HTMLElement | null
  if (props.autoActivate) activeIndex.value = nextSelectable(-1, 1)
  void nextTick(reposition)
  // Focus the menu itself. Without this aria-activedescendant names an element
  // inside something that never holds focus, which assistive tech ignores --
  // so the arrow keys moved the highlight visibly and silently.
  root.value?.focus()
  // Defer so the opening right-click's own mouseup/down doesn't immediately close it.
  setTimeout(() => {
    document.addEventListener('mousedown', onDocMouseDown)
  }, 0)
  document.addEventListener('keydown', onKeyDown)
})

onUnmounted(() => {
  document.removeEventListener('mousedown', onDocMouseDown)
  document.removeEventListener('keydown', onKeyDown)
  // Back where it came from, rather than falling to <body> and stranding a
  // keyboard user at the top of the page.
  if (opener !== null && document.contains(opener)) opener.focus()
})
</script>

<template>
  <ul
    ref="root"
    class="popup-menu"
    :style="style"
    role="menu"
    tabindex="-1"
    :aria-activedescendant="
      activeIndex >= 0 ? `${menuId}-${String(activeIndex)}` : undefined
    "
    @mousedown.stop
  >
    <template v-for="(item, i) in items" :key="i">
      <li v-if="item.separator" class="separator" role="separator"></li>
      <li
        v-else
        :id="`${menuId}-${String(i)}`"
        :role="item.checked === undefined ? 'menuitem' : 'menuitemcheckbox'"
        :class="{
          disabled: item.disabled,
          danger: item.danger,
          active: i === activeIndex,
        }"
        :aria-disabled="item.disabled"
        :aria-checked="item.checked"
        :aria-keyshortcuts="item.kbd"
        @click="choose(item)"
        @mousemove="activeIndex = isSelectable(item) ? i : -1"
      >
        <span v-if="hasChecks" class="check" aria-hidden="true">{{
          item.checked === true ? '✓' : ''
        }}</span>
        <span class="label">{{ item.label }}</span>
        <!-- Hidden from assistive tech, which is told the same thing by
             `aria-keyshortcuts` above; left visible it would append the chord
             to every item's spoken name. -->
        <span v-if="item.kbd" class="kbd" aria-hidden="true">{{ item.kbd }}</span>
      </li>
    </template>
  </ul>
</template>

<style scoped>
.popup-menu {
  position: fixed;
  z-index: var(--z-menu);
  min-width: 200px;
  margin: 0;
  padding: 0.25em 0;
  list-style: none;
  background: var(--surface);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-lg);
  font-size: var(--text-md);
  user-select: none;
  /* A menu taller than the window scrolls rather than running off it. */
  max-height: calc(100vh - 16px);
  overflow-y: auto;
}

/* The menu takes focus so its active item can be announced; the ring around
   the whole panel would just be noise on top of the highlighted row. */
.popup-menu:focus {
  outline: none;
}

li:not(.separator) {
  display: flex;
  align-items: baseline;
  padding: 0.3em 0.85em;
  cursor: pointer;
}

/* The pointer and the arrow keys highlight the same way, and `activeIndex`
   follows the pointer, so the two can never both look selected. */
li:not(.separator):hover:not(.disabled),
li.active {
  background: var(--surface-hover);
}

li.disabled {
  opacity: 0.4;
  cursor: default;
}

li.danger:not(.disabled) {
  color: var(--danger);
}

/* Pushes the shortcut hint to the right edge whatever the label's length. */
.label {
  flex: 1;
  white-space: nowrap;
}

/* Fixed width whether or not this row is ticked, so the labels of a menu
   holding toggles all start at the same place. */
.check {
  flex-shrink: 0;
  width: 1.2em;
}

.kbd {
  margin-left: 1.5em;
  font-size: 0.75rem;
  opacity: 0.6;
}

.separator {
  height: 1px;
  margin: 0.25em 0;
  background: var(--border);
}
</style>
