import { expect, test } from 'vitest'
import HtmlRenderer from '../../src/lpm/renderers/html'

// https://github.com/slag-plt/scamper/issues/530
//
// The element Scamper uses to show nothing set `display` to `'non'`, and the
// CSSOM drops an assignment whose value is not valid for the property --
// silently, since a style declaration is not a place an error can be raised.
// So the element kept a `div`'s default `display: block` and was never hidden
// at all.
//
// Nothing looked wrong at the top level, because an empty block `div` is zero
// pixels tall there either way; the difference shows where the element is
// nested in inline output, as in `(list (ignore 1) 2)`, where a block-level
// child splits the surrounding text across two lines.
//
// The element used to be `ignore`'s return value; #596 made `ignore` return
// void and moved the element to void's own rendering, so that is where the
// guarantee is pinned now.
//
// N.B., this file runs in the suite's default jsdom environment. jsdom
// implements the CSSOM's validation (and resolves `display` in
// getComputedStyle), so the bug is visible here; it implements no layout, so
// the line-breaking consequence above is not what these tests assert.

// The premise: an invalid value is dropped rather than stored, which is why the
// typo could not announce itself.
test('the CSSOM drops an invalid display value (#530)', () => {
  const probe = document.createElement('div')
  probe.style.display = 'non'
  expect(probe.style.display).toBe('')
  probe.style.display = 'none'
  expect(probe.style.display).toBe('none')
})

test('void renders as an element declared display: none (#530)', () => {
  expect(HtmlRenderer.render(undefined).style.display).toBe('none')
})

test("void's element is hidden once it is in the page (#530)", () => {
  const elt = HtmlRenderer.render(undefined)
  document.body.appendChild(elt)
  try {
    expect(getComputedStyle(elt).display).toBe('none')
  } finally {
    elt.remove()
  }
})
