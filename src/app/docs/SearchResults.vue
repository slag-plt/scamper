<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import DocEntry from './DocEntry.vue'
import { entryId, type LibEntry } from './modules'
import {
  filtersAreEmpty,
  noFilters,
  searchByFilters,
  searchByName,
  tagIsIndented,
  tagList,
  typeList,
  type Combinator,
  type Filters,
  type SearchRequest,
} from './search'

const props = defineProps<{ request: SearchRequest }>()

const term = computed(() => props.request.term)

/*
 * A search runs against a name or against the filters, never both -- the
 * filters describe the whole library rather than narrowing a name match. Which
 * one is showing is state, so that ticking a box does not discard the results
 * already on screen; only "Search" commits the panel.
 */
const draft = ref<Filters>(noFilters())
const committed = ref<Filters | null>(null)

// Every request from the header box (or from Back) is a name search, and takes
// over from whatever the filter panel last committed -- including a repeat of
// the term already in the box, which is the way back from a filter query.
watch(
  () => props.request,
  () => {
    committed.value = null
  },
)

const named = computed(() =>
  term.value === '' ? null : searchByName(term.value),
)

const results = computed<LibEntry[]>(() => {
  if (committed.value !== null) {
    return searchByFilters(committed.value)
  }
  return named.value === null
    ? []
    : [...named.value.matches, ...named.value.relatives]
})

/** Where the cross-referenced functions begin, or -1 if there are none. */
const relatedFrom = computed(() =>
  committed.value === null &&
  named.value !== null &&
  named.value.relatives.length > 0
    ? named.value.matches.length
    : -1,
)

const emptyMessage = computed(() => {
  if (results.value.length > 0) {
    return null
  }
  if (committed.value !== null) {
    return 'No results for the selected filters.'
  }
  return term.value === ''
    ? 'Type a function name in the search box, or pick filters below.'
    : `No results for “${term.value}”.`
})

function summarize(selected: string[]): string {
  return selected.length === 0 ? 'any' : selected.join(', ')
}

function commit() {
  committed.value = filtersAreEmpty(draft.value) ? null : snapshot(draft.value)
}

/** A copy, so that editing the panel afterwards does not alter the results. */
function snapshot(filters: Filters): Filters {
  return {
    argumentTypes: [...filters.argumentTypes],
    returnTypes: [...filters.returnTypes],
    tags: [...filters.tags],
    argumentMode: filters.argumentMode,
    tagMode: filters.tagMode,
  }
}

function reset() {
  draft.value = noFilters()
  committed.value = null
}

const modes: Combinator[] = ['or', 'and']
</script>

<template>
  <div class="search">
    <div class="filters">
      <h2>Filter</h2>

      <details class="filter">
        <summary>
          Arguments <span class="chosen">{{ summarize(draft.argumentTypes) }}</span>
        </summary>
        <label class="mode">
          match
          <select v-model="draft.argumentMode" class="text-input">
            <option v-for="mode in modes" :key="mode" :value="mode">
              {{ mode }}
            </option>
          </select>
        </label>
        <ul class="choices">
          <li v-for="type in typeList" :key="type">
            <label>
              <input v-model="draft.argumentTypes" type="checkbox" :value="type" />
              {{ type }}
            </label>
          </li>
        </ul>
      </details>

      <details class="filter">
        <summary>
          Returns <span class="chosen">{{ summarize(draft.returnTypes) }}</span>
        </summary>
        <ul class="choices">
          <li v-for="type in typeList" :key="type">
            <label>
              <input v-model="draft.returnTypes" type="checkbox" :value="type" />
              {{ type }}
            </label>
          </li>
        </ul>
      </details>

      <details class="filter">
        <summary>
          Tags <span class="chosen">{{ summarize(draft.tags) }}</span>
        </summary>
        <label class="mode">
          match
          <select v-model="draft.tagMode" class="text-input">
            <option v-for="mode in modes" :key="mode" :value="mode">
              {{ mode }}
            </option>
          </select>
        </label>
        <ul class="choices">
          <li
            v-for="tag in tagList"
            :key="tag"
            :class="{ sub: tagIsIndented(tag) }"
          >
            <label>
              <input v-model="draft.tags" type="checkbox" :value="tag" />
              {{ tag }}
            </label>
          </li>
        </ul>
      </details>

      <div class="actions">
        <button type="button" class="apply" @click="commit()">Search</button>
        <button type="button" class="clear" @click="reset()">Clear</button>
      </div>
    </div>

    <div class="results">
      <h2>
        Search results
        <span v-if="committed === null && term !== ''" class="term">
          for {{ term }}
        </span>
      </h2>
      <p v-if="emptyMessage" class="empty">{{ emptyMessage }}</p>
      <template v-for="(entry, i) in results" :key="entryId(entry)">
        <h3 v-if="i === relatedFrom" class="related">Related functions</h3>
        <DocEntry :id="entryId(entry)" :doc="entry.doc" />
      </template>
    </div>
  </div>
</template>

<style scoped>
.search {
  display: flex;
  flex-direction: row;
  flex: 1;
  min-height: 0;
  gap: var(--space-xl);
  padding: var(--space-xl);
}

h2 {
  margin: 0 0 var(--space-lg);
  font-size: var(--text-lg);
}

.filters {
  width: 17em;
  flex-shrink: 0;
  min-height: 0;
  overflow-y: auto;
  padding: var(--space-xl);
  background-color: var(--surface-muted);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
}

.filter {
  border-top: 1px solid var(--border);
  padding: var(--space-md) 0;
}

.filter summary {
  cursor: pointer;
  font-weight: bold;
}

.chosen {
  font-weight: normal;
  color: var(--syntax-comment);
}

.mode {
  display: flex;
  align-items: center;
  gap: var(--space-xs);
  padding: var(--space-md) 0;
  font-size: var(--text-sm);
}

.choices {
  list-style: none;
  margin: 0;
  padding: 0;
}

.choices li.sub {
  padding-left: var(--space-xl);
}

.choices label {
  display: flex;
  align-items: center;
  gap: var(--space-xs);
  padding: var(--space-xs) 0;
  font-size: var(--text-md);
  min-height: 1.5rem;
}

.actions {
  display: flex;
  gap: var(--space-md);
  padding-top: var(--space-xl);
  border-top: 1px solid var(--border);
}

.actions button {
  padding: var(--space-xs) var(--space-lg);
  font: inherit;
  font-size: var(--text-md);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  cursor: pointer;
}

.actions .apply {
  background: var(--accent);
  color: var(--accent-fg);
  border-color: transparent;
}

.actions .apply:hover {
  filter: brightness(1.08);
}

.actions .clear {
  background: var(--surface);
  color: inherit;
}

.actions .clear:hover {
  background: var(--surface-hover);
}

.results {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
}

.term {
  font-weight: normal;
  color: var(--syntax-comment);
}

.related {
  margin: var(--space-2xl) var(--space-xl) var(--space-md);
  font-size: var(--text-base);
}

.empty {
  margin: var(--space-xl);
}
</style>
