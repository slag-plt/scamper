import { mount } from '@vue/test-utils'
import { afterEach, describe, expect, test } from 'vitest'
import ApiEntries from '../../src/app/docs/ApiEntries.vue'
import DocsApp from '../../src/app/docs/DocsApp.vue'
import { docRegistry, moduleDocRegistry } from '../../src/lib'
import type { FunctionDoc } from '../../src/scheme/docstring/docstring'

// initializeLibs() has already run: test/setup.ts does it globally.

/** The `prelude` entries, so the page under test is the real one. */
function preludeLib(): Map<string, FunctionDoc> {
  return docRegistry.get('prelude') ?? new Map<string, FunctionDoc>()
}

// What a module is for, shown above what is in it (#411). No library carries a
// module comment yet, so the empty case here is the shipped one -- which is
// exactly why it is the case worth pinning.
describe('the module comment on the docs page', () => {
  afterEach(() => {
    document.body.innerHTML = ''
  })

  test('is shown above the entries when the module has one', () => {
    const wrapper = mount(ApiEntries, {
      attachTo: document.body,
      props: {
        moduleName: 'prelude',
        lib: preludeLib(),
        moduleDoc: { description: 'The functions every program starts with.' },
      },
    })
    try {
      const blurb = document.querySelector('.module-doc')
      expect(blurb?.textContent).toContain(
        'The functions every program starts with.',
      )
      // Above the entry cards, not among them.
      const entries = document.querySelector('.entries')
      expect(entries?.firstElementChild).toBe(blurb)
    } finally {
      wrapper.unmount()
    }
  })

  test('renders backticked text as code, as a description does', () => {
    const wrapper = mount(ApiEntries, {
      attachTo: document.body,
      props: {
        moduleName: 'prelude',
        lib: preludeLib(),
        moduleDoc: { description: 'Start with `map` and `filter`.' },
      },
    })
    try {
      const code = [...document.querySelectorAll('.module-doc code')].map(
        (el) => el.textContent,
      )
      expect(code).toEqual(['map', 'filter'])
    } finally {
      wrapper.unmount()
    }
  })

  test('is absent entirely when the module has none', () => {
    // Not an empty paragraph or a gap -- nothing at all, since that is how
    // every module renders today.
    const wrapper = mount(ApiEntries, {
      attachTo: document.body,
      props: { moduleName: 'prelude', lib: preludeLib() },
    })
    try {
      expect(document.querySelector('.module-doc')).toBeNull()
      // The first thing under the entries is still an entry.
      const entries = document.querySelector('.entries')
      expect(entries?.firstElementChild?.classList.contains('entry')).toBe(true)
    } finally {
      wrapper.unmount()
    }
  })

  test('the page shows the selected module’s comment', () => {
    // Through DocsApp, so the registry lookup and the prop are wired together
    // rather than each being right on its own.
    moduleDocRegistry.set('prelude', {
      description: 'The functions every program starts with.',
    })
    const wrapper = mount(DocsApp, { attachTo: document.body })
    try {
      expect(document.querySelector('.module-doc')?.textContent).toContain(
        'The functions every program starts with.',
      )
    } finally {
      wrapper.unmount()
      moduleDocRegistry.delete('prelude')
    }
  })

  test('the page shows none when the module has none', () => {
    const wrapper = mount(DocsApp, { attachTo: document.body })
    try {
      expect(document.querySelector('.module-doc')).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })
})
