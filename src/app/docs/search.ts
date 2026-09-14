/**
 * Searching and filtering the standard library. The rules came over from the
 * standalone search page (#403) as plain functions rather than side effects of
 * rendering; name matching has since widened to a substring (#603).
 */

import {
  functionDocCategories,
  functionDocName,
  predTypeName,
} from '../../scheme/docstring/render'
import { allEntries, entryId, type LibEntry } from './modules'
import type { FunctionDoc } from '../../scheme/docstring/docstring'

/** Every documented param's predicate, including the rest param if any. */
function argPredicates(doc: FunctionDoc) {
  return [
    ...doc.params,
    ...doc.optParams,
    ...(doc.restParam ? [doc.restParam] : []),
  ].map(
    (p) => p.predicate,
  )
}

/**
 * The tags a function can be filtered by. Indented entries are subcategories
 * of the entry above them, which is what `tagIsIndented` reports.
 */
export const tagList = [
  'char',
  'string',
  'list',
  'association list',
  'list creation',
  'list manipulation',
  'vectors',
  'hashmap',
  'mutation',
  'constants',
  'function composition',

  'math',
  'algebra',
  'trigonometry',
  'comparator',
  'boolean/logic',

  'images',
  'color',
  'pixel',
  'rgb',
  'hsv',

  'composition/placement',
  'path',
  'canvas',
  'shapes',

  'music',
  'duration',
  'instruments',
  'note',
  'modifications',
  'audio',
  'sound',

  'data',
  'create',
  'plot',
  'parse',

  'file',

  'typecheck',

  'regexes',

  'predicates',
  'testing',
  'formatting',

  'interactive',
  'html',
  'reactive',

  'other',
]

const indentedTags = new Set([
  'association list',
  'list creation',
  'list manipulation',
  'algebra',
  'trigonometry',
  'comparator',
  'boolean/logic',
  'color',
  'rgb',
  'hsv',
  'shapes',
  'composition/placement',
  'path',
  'pixel',
  'duration',
  'instruments',
  'note',
  'modifications',
  'sound',
  'create',
  'plot',
  'parse',
])

/** Whether `tag` is a subcategory of the tag above it in `tagList`. */
export function tagIsIndented(tag: string): boolean {
  return indentedTags.has(tag)
}

/** The types a function's arguments or result can be filtered by. */
export const typeList = [
  'any',
  'audio-node',
  'audio',
  'boolean',
  'canvas',
  'char',
  'color',
  'composition',
  'context',
  'drawing',
  'duration',
  'element',
  'font',
  'function',
  'hsv',
  'integer',
  'list',
  'mod',
  'number',
  'procedure',
  'ref',
  'rex',
  'rgb',
  'rgb-component',
  'string',
  'text-area',
  'vector',
]

/**
 * A few `@category` tags name a concept whose functions are spelled
 * differently, so a docstring saying `rgb-func` means "see `rgb`".
 */
const relatedTagAliases: Record<string, string> = {
  'rgb-func': 'rgb',
  'hsv-func': 'hsv',
  'color-func': 'color',
  'path-func': 'path',
  'string-func': 'string',
  'tag-func': 'tag',
}

/**
 * The functions `doc` points at. A `@category` that is not one of the filter
 * tags is read as the name of a related function -- that is how a docstring
 * cross-references, and the convention predates this page.
 */
function relativesOf(doc: FunctionDoc): LibEntry[] {
  const names = functionDocCategories(doc)
    .filter((tag) => !tagList.includes(tag))
    .map((tag) => relatedTagAliases[tag] ?? tag)
  return allEntries().filter((e) => names.includes(functionDocName(e.doc)))
}

/** What a name search turned up: the matches, then their cross-references. */
export interface NameResults {
  matches: LibEntry[]
  relatives: LibEntry[]
}

/** How well `name` answers `term`: 0 exact, 1 a prefix, 2 merely contains it. */
function nameRank(name: string, term: string): number {
  if (name === term) {
    return 0
  }
  return name.startsWith(term) ? 1 : 2
}

/**
 * A name search: every function whose name contains `name`, ignoring case,
 * best answer first (#603). It matched only a whole name until then, so a
 * student typing what they remembered -- `image-save` for `image-save!` --
 * found nothing.
 *
 * Cross-references come from an *exact* match only. They are shown under a
 * divider reading "here is the function, here is its neighbourhood", which says
 * nothing once the term is a fragment matching twenty functions.
 *
 * A docstring may list its own function among its `@category` entries -- five
 * do, `string-length` among them -- so the relatives are deduplicated against
 * the matches, and against each other when two matches point at the same
 * function. Left in, the same entry rendered twice under one id.
 */
export function searchByName(name: string): NameResults {
  const term = name.toLowerCase()
  // "" is a substring of every name, and the whole library is not a search
  // result. The docs page never asks -- it treats an empty box as no search --
  // so this is the function's own answer rather than a second guard.
  if (term === '') {
    return { matches: [], relatives: [] }
  }
  const nameOf = (e: LibEntry) => functionDocName(e.doc).toLowerCase()
  // `sort` is stable, so entries within a band keep the order the library
  // lists them in.
  const matches = allEntries()
    .filter((e) => nameOf(e).includes(term))
    .sort((a, b) => nameRank(nameOf(a), term) - nameRank(nameOf(b), term))
  const seen = new Set(matches.map(entryId))
  const relatives: LibEntry[] = []
  const exact = matches.filter((e) => nameOf(e) === term)
  for (const entry of exact.flatMap((e) => relativesOf(e.doc))) {
    const id = entryId(entry)
    if (!seen.has(id)) {
      seen.add(id)
      relatives.push(entry)
    }
  }
  return { matches, relatives }
}

/**
 * One press of Enter in the search box. The docs page makes a fresh object
 * every time, including for a term the box already holds, so that repeating a
 * search still counts as a new request and clears any committed filters.
 */
export interface SearchRequest {
  term: string
}

/** How a filter combines the values selected within it. */
export type Combinator = 'or' | 'and'

/** Everything the filter panel can constrain a search by. */
export interface Filters {
  argumentTypes: string[]
  returnTypes: string[]
  tags: string[]
  argumentMode: Combinator
  tagMode: Combinator
}

/** A filter set that constrains nothing. */
export function noFilters(): Filters {
  return {
    argumentTypes: [],
    returnTypes: [],
    tags: [],
    argumentMode: 'or',
    tagMode: 'or',
  }
}

/** Whether any of `filters` actually narrows the results. */
export function filtersAreEmpty(filters: Filters): boolean {
  return (
    filters.argumentTypes.length === 0 &&
    filters.returnTypes.length === 0 &&
    filters.tags.length === 0
  )
}

function matchesAny(selected: string[], has: (t: string) => boolean): boolean {
  return selected.length === 0 || selected.some(has)
}

function matchesAll(selected: string[], has: (t: string) => boolean): boolean {
  // A section with nothing ticked is unset, whichever mode its dropdown is on.
  // "All of nothing" was false here, so opening a section, switching it to
  // "and", and ticking nothing in it quietly emptied the whole result set --
  // including whatever a *different* section had selected (#408).
  return selected.length === 0 || selected.every(has)
}

/** The functions passing every filter in `filters`. */
export function searchByFilters(filters: Filters): LibEntry[] {
  return allEntries().filter(({ doc }) => {
    const argTypes = argPredicates(doc).map(predTypeName)
    const docTags = functionDocCategories(doc)
    const byArgs =
      filters.argumentMode === 'or'
        ? matchesAny(filters.argumentTypes, (t) => argTypes.includes(t))
        : matchesAll(filters.argumentTypes, (t) => argTypes.includes(t))
    const byTags =
      filters.tagMode === 'or'
        ? matchesAny(filters.tags, (t) => docTags.includes(t))
        : matchesAll(filters.tags, (t) => docTags.includes(t))
    const byReturn =
      filters.returnTypes.length === 0 ||
      filters.returnTypes.includes(predTypeName(doc.signature.predicate))
    return byReturn && byArgs && byTags
  })
}
