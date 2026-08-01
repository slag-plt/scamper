<script setup lang="ts">
import AppModal from './AppModal.vue'
import type { PatchNote } from '../patch-notes'

// A one-time "what's new" dialog (issue #306), shown when a user opens a version
// of Scamper they haven't seen. Built on the generic AppModal (#305). The parent
// (IdeApp) owns the `open` state and records the seen version on close.
defineProps<{ open: boolean; notes: PatchNote[] }>()
const emit = defineEmits<{ close: [] }>()
</script>

<template>
  <AppModal :open="open" title="What's new in Scamper" @dismiss="emit('close')">
    <div class="patch-notes">
      <section v-for="note in notes" :key="note.version" class="patch-note">
        <h3 class="patch-note__version">
          Version {{ note.version
          }}<span v-if="note.title"> — {{ note.title }}</span>
        </h3>
        <ul class="patch-note__list">
          <li v-for="(item, i) in note.notes" :key="i">{{ item }}</li>
        </ul>
      </section>
    </div>
    <template #footer>
      <button type="button" class="patch-notes__button" autofocus @click="emit('close')">
        Got it
      </button>
    </template>
  </AppModal>
</template>

<style scoped>
.patch-notes {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  max-height: 60vh;
  overflow-y: auto;
}

.patch-note__version {
  margin: 0 0 0.35rem;
  font-size: 1rem;
  font-weight: 600;
}

.patch-note__list {
  margin: 0;
  padding-left: 1.25rem;
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
}

.patch-notes__button {
  padding: 0.4rem 0.9rem;
  border: 1px solid var(--accent);
  border-radius: 6px;
  background-color: var(--accent);
  color: var(--accent-fg);
  font: inherit;
  cursor: pointer;
}

.patch-notes__button:hover {
  filter: brightness(1.05);
}
</style>
