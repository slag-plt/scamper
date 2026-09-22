import { readFileSync } from 'fs'
import { resolve } from 'path'
import { mount } from '@vue/test-utils'
import { JSDOM } from 'jsdom'
import { describe, expect, test } from 'vitest'
import SearchResults from '../../src/app/docs/SearchResults.vue'
import { tokenizeAndParse } from '../../src/scheme'
import { reservedWords } from '../../src/scheme/reserved-words'

// Regression tests for #543: the special forms used to appear in the function
// documentation as callable stubs -- `if☀︎`, `and☀︎`, `or☀︎`, `apply☀︎` -- which
// were all bound to `equal?` and so answered wrongly. They are described on the
// language reference page instead, one section per form, and these tests are
// what keep that page in step with the language: they are the same parity
// argument test/scheme/parsing/grammar-keyword-parity.test.ts makes between
// `reservedWords` and the grammar, made here between `reservedWords` and the
// page.
//
// The page is read the way a browser reads it, not as text, because that is
// what notices a sample whose contents were swallowed as markup (#543 fixed a
// literal `<body>` inside a `<pre>`, which the browser ate along with the rest
// of the sentence it sat in).

const source = readFileSync(
  resolve(__dirname, '../../public/reference.html'),
  'utf-8',
)
const page = new JSDOM(source).window.document

/** A section's form, as the page renders it. */
function formOf(section: Element): string {
  return section.querySelector('h3')?.textContent.trim() ?? ''
}

const sections = [...page.querySelectorAll('section[id]')]

/** The forms that earn a section without being reserved words. */
const otherForms = [
  'names',
  'literals',
  ';',
  'procedure call',
  '&',
  '[...]',
  '{...}',
  '#(...)',
  'patterns',
]

describe('language reference page', () => {
  test('every reserved word has a section, and every section names a real form', () => {
    const forms = new Set(sections.map(formOf))
    const known = new Set([...reservedWords, ...otherForms])

    const missing = reservedWords.filter((w) => !forms.has(w))
    const unknown = [...forms].filter((f) => !known.has(f))

    expect(
      missing,
      `reserved words with no section in public/reference.html: ${missing.join(', ')}`,
    ).toEqual([])
    expect(
      unknown,
      `sections of public/reference.html naming no form of the language: ${unknown.join(', ')}`,
    ).toEqual([])
  })

  test("a reserved word's section is anchored by the word itself", () => {
    // src/app/docs/SearchResults.vue links reference.html#<term> when a search
    // term is a reserved word, so the id is part of the interface.
    const unanchored = reservedWords.filter((w) => page.getElementById(w) === null)
    expect(
      unanchored,
      `reserved words with no matching id= anchor: ${unanchored.join(', ')}`,
    ).toEqual([])
  })

  test('every code sample renders exactly what the file says', () => {
    // An unescaped "<" inside a <pre> is markup, not text: `<body>` is dropped
    // outright, silently taking the rest of its sentence with it. Comparing
    // what the browser renders against what the file holds is what notices.
    // A newline straight after `<pre>` is part of the tag, not the text, so
    // the browser drops it; the file is compared as the browser reads it.
    const written = [...source.matchAll(/<pre[^>]*>\n?([\s\S]*?)<\/pre>/g)].map(
      (m) => unescape(m[1]),
    )
    const rendered = [...page.querySelectorAll('pre')].map((p) => p.textContent)
    expect(rendered).toEqual(written)
  })

  test('every section shows its syntax, prose, and an example that parses', () => {
    for (const section of sections) {
      const form = formOf(section)
      expect(section.querySelector('pre.syntax')?.textContent, form).toBeTruthy()
      expect(section.querySelector('p')?.textContent, form).toBeTruthy()

      const examples = [...section.querySelectorAll('pre.example')]
      expect(examples.length, `${form} has no example`).toBeGreaterThan(0)
      for (const example of examples) {
        const src = example.textContent
        const { diagnostics } = tokenizeAndParse(src)
        expect(
          diagnostics.map((d) => d.message),
          `${form}: example does not parse: ${src}`,
        ).toEqual([])
      }
    }
  })
})

describe('searching the docs for a special form', () => {
  test('points at the reference page instead of a procedure', () => {
    // Deleting the stubs would otherwise leave `apply` with no hit at all and
    // `if` with only the functions whose names happen to contain "if".
    const wrapper = mount(SearchResults, { props: { request: { term: 'if' } } })
    const link = wrapper.get('.special-form a')
    expect(wrapper.get('.special-form').text()).toContain('special form')
    expect(link.attributes('href')).toBe('reference.html#if')
  })

  test('says nothing when the term is an ordinary procedure', () => {
    const wrapper = mount(SearchResults, { props: { request: { term: 'car' } } })
    expect(wrapper.find('.special-form').exists()).toBe(false)
  })
})

/** The five entities a `<pre>`'s contents have to be written with. */
function unescape(html: string): string {
  return html
    .replaceAll('&lt;', '<')
    .replaceAll('&gt;', '>')
    .replaceAll('&quot;', '"')
    .replaceAll('&#39;', "'")
    .replaceAll('&amp;', '&')
}
