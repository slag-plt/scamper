import { mount } from '@vue/test-utils'
import { beforeEach, describe, expect, test } from 'vitest'
import { EMBED_CLASS, runEmbeds } from '../../src/app/web/embed/embed'
import HtmlRenderer from '../../src/lpm/renderers/html'
import TextRenderer from '../../src/lpm/renderers/text'
import ValueRenderer from '../../src/lpm/renderers/vue/ValueRenderer.vue'
import * as U from '../../src/lpm/util'
import { initialize } from '../../src/scamper'
import { query } from '../dom'
import '../../src/app/web/renderers'

await initialize()

// https://github.com/slag-plt/scamper/issues/668
//
// #612 aligned the IDE and the CLI on spelling the empty list `null`, and missed
// `src/lpm/renderers/html.ts` -- the renderer behind the embedded transcript
// widget -- so a reading went on showing `()`. The divergence did not close; it
// moved to the surface a student meets in a reading rather than in the editor.
//
// This is not one renderer's defensible second opinion. `docs/DIFFERENCES.md`
// states the language design outright: "The empty list is `null`, not `'()`" --
// necessarily, since Scamper has no quotation. So `()` contradicted the
// documented design, and there is nothing here to reconcile.
//
// Three renderers draw a value, so all three are pinned here *together* rather
// than only the one that had drifted. Spelling this out in one place is what
// keeps a fourth surface from reopening the question a third time.
describe('#668: the empty list is spelled null on every surface', () => {
  test('toString and the text renderer', () => {
    expect(U.toString(null)).toBe('null')
    expect(TextRenderer.render(null)).toBe('null')
  })

  test('the html renderer', () => {
    expect(HtmlRenderer.render(null).textContent).toBe('null')
  })

  // N.B., `empty-string-render.test.ts` also happens to cover null through
  // ValueRenderer, as one case in #444's list of values it must not mangle. The
  // overlap is deliberate: that file pins the Vue renderer against *its* bug,
  // whereas the point of #668 is that the three surfaces agree, which only
  // reads as a rule if they are asserted side by side.
  test('the vue renderer', () => {
    expect(mount(ValueRenderer, { props: { value: null } }).text()).toBe('null')
  })

  // Nested, since that is how a student usually meets the empty list: inside
  // something else, rather than as a whole result. Only HtmlRenderer is checked
  // this way -- the Vue renderer joins nested values without separators, so its
  // spelling of a list is a different assertion entirely.
  test('nested inside a list, in the html renderer', () => {
    expect(HtmlRenderer.render(U.mkList(1, null)).textContent).toBe(
      '(list 1 null)',
    )
  })
})

// The surface the issue was reported from, end to end: not the renderer in
// isolation but a reading widget running `(list)`, the program a student wrote.
describe('#668: a reading widget running (list)', () => {
  beforeEach(() => {
    document.body.innerHTML = ''
  })

  test('shows null, and nowhere shows ()', async () => {
    document.body.innerHTML = `<div class="${EMBED_CLASS}">(list)</div>`
    const el = query(document.body, `.${EMBED_CLASS}`)
    await runEmbeds()

    expect(query(el, '.scamper-output').textContent).toContain('null')
    // Safe as a negative over the whole widget: the source text `(list)` holds
    // no `()` of its own, so the only way this matches is the old spelling.
    expect(el.textContent).not.toContain('()')
  })
})
