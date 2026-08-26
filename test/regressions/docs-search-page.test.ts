import { mount } from '@vue/test-utils'
import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import { readFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import DocsApp from '../../src/app/docs/DocsApp.vue'
import DocEntry from '../../src/app/docs/DocEntry.vue'
import { entryId } from '../../src/app/docs/modules'
import { functionDocName } from '../../src/scheme/docstring/render'
import {
  noFilters,
  searchByFilters,
  searchByName,
  tagList,
  typeList,
} from '../../src/app/docs/search'
import { initializeLibs } from '../../src/lib'

await initializeLibs()

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), '../..')

function source(path: string): string {
  return readFileSync(resolve(repoRoot, path), 'utf-8')
}

// Regression tests for #403: search lived in its own app, a fork of the docs
// app that missed the UI overhaul. Its header was filled with --brand (the
// accent colour) rather than --header-bg, so the nav links -- still --link
// blue -- sat unreadably on saturated teal, and its controls predated the
// design tokens entirely: inline `helvetica`, negative margins, an "Enter"
// button hardcoded to 88x188px at 40px.
//
// The fork is the cause, so the fix is to merge: the docs app owns search, and
// search.html is a redirect into it. What follows pins both halves -- that the
// two views render from one app, and that the styling drift cannot return.

function mountDocs(search: string) {
  window.history.replaceState({}, '', `/docs.html${search}`)
  return mount(DocsApp, { attachTo: document.body })
}

describe('#403: search is part of the docs app', () => {
  beforeEach(() => {
    window.history.replaceState({}, '', '/docs.html')
  })

  afterEach(() => {
    document.body.innerHTML = ''
    window.history.replaceState({}, '', '/docs.html')
  })

  test('the standalone search app is gone', () => {
    // search.html survives as a redirect so existing links and bookmarks keep
    // working, but it must no longer boot a second copy of the docs UI.
    const html = source('src/app/search/search.html')
    expect(html).toContain('docs.html')
    expect(html).not.toContain('src/app/search/index.ts')
  })

  test('?search= renders results inside the docs app', () => {
    const wrapper = mountDocs('?search=map')
    expect(wrapper.text()).toContain('(map f')
    // The results replace the module browser rather than sitting alongside it.
    expect(wrapper.find('.modules').exists()).toBe(false)
  })

  test('a search that matches nothing says so', () => {
    const wrapper = mountDocs('?search=definitely-not-a-function')
    expect(wrapper.text()).toContain('No results')
    // The old page fell through to showEverything(), dumping the entire
    // standard library whenever the result set was empty.
    expect(wrapper.findAllComponents(DocEntry)).toHaveLength(0)
  })

  test('an empty search does not dump the whole library', () => {
    const wrapper = mountDocs('?search=')
    expect(wrapper.findAllComponents(DocEntry)).toHaveLength(0)
  })

  test('no ?search= shows the module browser', () => {
    const wrapper = mountDocs('')
    expect(wrapper.text()).toContain('prelude')
    expect(wrapper.text()).toContain('image')
  })

  test('a new term takes over from a committed filter search', async () => {
    const wrapper = mountDocs('?search=map')
    const tag = wrapper
      .findAll('input[type="checkbox"]')
      .find((i) => i.element.getAttribute('value') === 'trigonometry')
    await tag?.setValue(true)
    await wrapper.get('button.apply').trigger('click')
    expect(wrapper.text()).toContain('(sin v)')

    // Searching by name again has to win. The filter panel used to keep its
    // committed query forever, so the header box silently stopped working.
    const box = wrapper.get('input.text-input')
    await box.setValue('circle')
    await box.trigger('keyup.enter')
    expect(wrapper.text()).toContain('for circle')
    expect(wrapper.text()).not.toContain('(sin v)')
  })

  test('pressing enter on the same term returns from a filter search', async () => {
    // The way back from a filter query is the term already in the box, so a
    // repeat of it has to count as a new request rather than a no-op.
    const wrapper = mountDocs('?search=map')
    const tag = wrapper
      .findAll('input[type="checkbox"]')
      .find((i) => i.element.getAttribute('value') === 'trigonometry')
    await tag?.setValue(true)
    await wrapper.get('button.apply').trigger('click')
    expect(wrapper.text()).toContain('(sin v)')

    const box = wrapper.get('input.text-input')
    expect((box.element as HTMLInputElement).value).toBe('map')
    await box.trigger('keyup.enter')
    expect(wrapper.text()).toContain('(map f')
    expect(wrapper.text()).not.toContain('(sin v)')
  })

  test('results carry unique ids even across modules', () => {
    // `square`, `html?` and four others are exported by more than one module,
    // so a bare function name is not a usable key or anchor.
    const entries = searchByFilters({ ...noFilters(), returnTypes: ['boolean'] })
    const ids = entries.map(entryId)
    expect(new Set(ids).size).toBe(ids.length)
  })
})

// The matching rules came from the standalone page as student-written code and
// were carried over deliberately unchanged, so these pin the behaviour itself
// rather than any particular spelling of it.
describe('#403: the search rules survived the move', () => {
  test('a name search returns the exact match, then its cross-references', () => {
    const { matches, relatives } = searchByName('map')
    expect(matches.map((e) => functionDocName(e.doc))).toEqual(['map'])
    expect(relatives.map((e) => functionDocName(e.doc))).toEqual([
      'string-map',
      'reduce',
      'reduce-right',
      'vector-map',
      'vector-map!',
      'set-maximum-recursion-depth!',
    ])
  })

  test('only an exact name matches', () => {
    expect(searchByName('ma').matches).toHaveLength(0)
    expect(searchByName('').matches).toHaveLength(0)
  })

  test('an unset filter constrains nothing in "or" mode', () => {
    expect(searchByFilters(noFilters()).length).toBeGreaterThan(400)
  })

  test('an unset filter matches nothing in "and" mode', () => {
    // Carried over verbatim: "all of nothing" is false, not true.
    const filters = noFilters()
    expect(searchByFilters({ ...filters, argumentMode: 'and' })).toHaveLength(0)
    expect(searchByFilters({ ...filters, tagMode: 'and' })).toHaveLength(0)
  })

  test('tag and type filters still select what they used to', () => {
    const trig = searchByFilters({ ...noFilters(), tags: ['trigonometry'] })
    expect(trig.map((e) => functionDocName(e.doc))).toContain('sin')
    expect(trig.map((e) => functionDocName(e.doc))).toContain('acos')

    const takesChars = searchByFilters({
      ...noFilters(),
      argumentTypes: ['char'],
    })
    expect(takesChars.length).toBeGreaterThan(0)
    expect(takesChars.map((e) => functionDocName(e.doc))).not.toContain('sin')
  })

  test('a function that cross-references itself is listed once', () => {
    // Five docstrings name their own function among their @category entries,
    // which rendered the entry twice under one id.
    for (const name of [
      'string-length',
      'list->vector',
      'rgb-lighter',
      'pickup',
      'rex-empty',
    ]) {
      const { matches, relatives } = searchByName(name)
      const ids = [...matches, ...relatives].map(entryId)
      expect(new Set(ids).size, name).toBe(ids.length)
      expect(matches.length, name).toBeGreaterThan(0)
    }
  })

  test('the filter vocabularies came across whole', () => {
    expect(tagList).toHaveLength(46)
    expect(typeList).toHaveLength(27)
    expect(new Set(tagList).size).toBe(tagList.length)
    expect(new Set(typeList).size).toBe(typeList.length)
  })
})

describe('#403: the docs pages are on the design tokens', () => {
  const components = [
    'src/app/docs/DocsApp.vue',
    'src/app/docs/ApiEntries.vue',
    'src/app/docs/SearchResults.vue',
    'src/app/docs/DocEntry.vue',
    'src/app/docs/ModuleList.vue',
  ]

  test('the header uses --header-bg, not the brand accent', () => {
    const app = source('src/app/docs/DocsApp.vue')
    expect(app).toContain('background: var(--header-bg)')
    expect(app).not.toMatch(/background:\s*var\(--brand\)/)
  })

  test('the search box is a shared .text-input', () => {
    const app = source('src/app/docs/DocsApp.vue')
    expect(app).toMatch(/class="text-input"/)
  })

  test('no component hardcodes a font family', () => {
    for (const path of components) {
      const text = source(path)
      expect(text, path).not.toMatch(/font-family:[^;]*helvetica/i)
      expect(text, path).not.toMatch(/font-family:[^;]*Menlo/)
    }
  })

  test('no component lays itself out with negative margins', () => {
    for (const path of components) {
      const text = source(path)
      expect(text, path).not.toMatch(/margin[a-z-]*:\s*-/)
    }
  })

  test('the filter panel uses real elements and real controls', () => {
    const results = source('src/app/docs/SearchResults.vue')
    // <text> is not an HTML element; the browser parsed it as an unknown
    // inline box, which is why the filter labels ignored every style.
    expect(results).not.toMatch(/<text[\s>]/)
    // The "Enter" button was 88x188px at 40px monospace.
    expect(results).not.toMatch(/font-size:\s*40px/)
  })

  test('the filter panel does not print its state as debug text', () => {
    const results = source('src/app/docs/SearchResults.vue')
    expect(results).not.toMatch(/Types:\{\{/)
    expect(results).not.toMatch(/Tags:\{\{/)
  })
})
