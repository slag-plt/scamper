import { expect, test } from 'vitest'
import { runProgramValues } from '../harness'

// https://github.com/slag-plt/scamper/issues/530
//
// `ignore` exists to swallow a value without showing it, and does so by handing
// back an empty `div` that takes up no space. It set `display` to `'non'`, and
// the CSSOM drops an assignment whose value is not valid for the property --
// silently, since a style declaration is not a place an error can be raised. So
// the element kept a `div`'s default `display: block` and was never hidden at
// all.
//
// Nothing looked wrong at the top level, because an empty block `div` is zero
// pixels tall there either way; the difference shows where the element is
// nested in inline output, as in `(list (ignore 1) 2)`, where a block-level
// child splits the surrounding text across two lines.
//
// N.B., this file runs in the suite's default jsdom environment. jsdom
// implements the CSSOM's validation (and resolves `display` in
// getComputedStyle), so the bug is visible here; it implements no layout, so
// the line-breaking consequence above is not what these tests assert.

/** The element `(ignore v)` hands back. */
async function ignoredElement(): Promise<HTMLElement> {
  const out = await runProgramValues('(ignore 5)')
  expect(out).toHaveLength(1)
  const v = out[0]
  expect(v).toBeInstanceOf(HTMLElement)
  return v as HTMLElement
}

// The premise: an invalid value is dropped rather than stored, which is why the
// typo could not announce itself.
test('the CSSOM drops an invalid display value (#530)', () => {
  const probe = document.createElement('div')
  probe.style.display = 'non'
  expect(probe.style.display).toBe('')
  probe.style.display = 'none'
  expect(probe.style.display).toBe('none')
})

test('ignore returns an element declared display: none (#530)', async () => {
  const elt = await ignoredElement()
  expect(elt.style.display).toBe('none')
})

test("ignore's element is hidden once it is in the page (#530)", async () => {
  const elt = await ignoredElement()
  document.body.appendChild(elt)
  try {
    expect(getComputedStyle(elt).display).toBe('none')
  } finally {
    elt.remove()
  }
})
