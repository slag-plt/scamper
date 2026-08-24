<script setup lang="ts">
// Bottom bar showing the cursor's line/column and the breadcrumb of syntactic
// forms enclosing it, outermost first, e.g.
// "Ln 4, Col 8: define > lambda > cond > application".
// The account and connection controls used to live here; they moved into the
// file drawer (IdeSidebar.vue), where the files they govern are.
defineProps<{
  line: number
  column: number
  path: string[]
  /** True for a file shown rather than edited -- see isHiddenName (#178). */
  readOnly?: boolean
}>()
</script>

<template>
  <div class="ide-statusbar">
    <span v-if="readOnly" class="read-only">Read-only</span>
    <span class="pos">Ln {{ line }}, Col {{ column }}:</span>
    <span v-if="path.length === 0" class="muted">top level</span>
    <template v-for="(form, i) in path" :key="i">
      <span v-if="i > 0" class="sep" aria-hidden="true">›</span>
      <span class="crumb">{{ form }}</span>
    </template>
  </div>
</template>

<style scoped>
.ide-statusbar {
  flex-shrink: 0;
  background: var(--surface-muted);
  color: var(--fg);
  border-top: 1px solid var(--border-muted);
  padding: 0.2em 0.6em;
  font-family:
    Menlo, Consolas, Monaco, "Liberation Mono", "Lucida Console", monospace;
  font-size: 0.8em;
  line-height: 1.4;
  white-space: nowrap;
  overflow-x: auto;
}

.pos {
  margin-right: 0.5em;
}

/*
 * Says why typing does nothing, rather than leaving it to be discovered. An
 * internal file (a saved history, a config) is shown for looking at; writing
 * one back would destroy the state it holds.
 */
.read-only {
  margin-right: 0.6em;
  padding: 0 0.4em;
  border: 1px solid var(--border-muted);
  border-radius: 3px;
  text-transform: uppercase;
  font-size: 0.85em;
  letter-spacing: 0.04em;
  opacity: 0.85;
}

.sep {
  margin: 0 0.4em;
  opacity: 0.5;
}

.muted {
  opacity: 0.6;
  font-style: italic;
}

</style>
