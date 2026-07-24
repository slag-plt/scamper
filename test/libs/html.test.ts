import { describe, expect, test, vi } from 'vitest'
import { runProgram } from '../harness.js'
import {
  html_button,
  html_buttonQ,
  html_isElement,
  html_onKeydown,
  html_tag,
  html_tagSetChildren,
  html_textArea,
  html_textAreaGet,
  html_textAreaQ,
} from '../../src/js/html/index.js'
import * as L from '../../src/lpm/index.js'

// html.scm binds directly to the DOM. An HTMLElement always prints as
// '[HTMLElement]' via runProgram regardless of its actual shape, so tests
// that need to inspect an element's tag/attributes/children go through the
// html_* functions directly instead.

describe('element?', () => {
  test('is true for any HTML element', async () => {
    expect(await runProgram(`
    (import html)
    (element? (tag "div"))
    (element? (button "Click" (lambda () #t)))
    `)).toEqual(['#t', '#t'])
  })

  test('is false for non-element values', async () => {
    expect(await runProgram(`
    (import html)
    (element? 5)
    (element? "div")
    (element? #t)
    (element? (list 1 2 3))
    (element? (pair 1 2))
    `)).toEqual(['#f', '#f', '#f', '#f', '#f'])
  })
})

describe('text-area?', () => {
  // N.B., positive case can't go through the Scamper-level `text-area`
  // here -- see the `text-area` describe block below.
  test('is true for an HTMLTextAreaElement', () => {
    expect(html_textAreaQ(document.createElement('textarea'))).toBe(true)
  })

  test('is false for non-text-area values', () => {
    expect(html_textAreaQ(html_tag('div'))).toBe(false)
    expect(html_textAreaQ(5)).toBe(false)
    expect(html_textAreaQ('textarea')).toBe(false)
    expect(html_textAreaQ(null)).toBe(false)
  })
})

describe('text-area', () => {
  // html_textArea builds its result via `new HTMLTextAreaElement()`, unlike
  // html_tag/html_button which correctly use document.createElement. The
  // HTMLConstructor behavior throws "Illegal constructor" in every environment
  // (real browsers and jsdom alike), so text-area is broken everywhere, not
  // just here -- github.com/slag-plt/scamper#260.
  test('always throws -- new HTMLTextAreaElement() is illegal (see #260)', () => {
    expect(() => html_textArea('my-id')).toThrow()
  })
})

describe('text-area-get', () => {
  test('returns the text area\'s current text content', () => {
    const ta = document.createElement('textarea')
    ta.textContent = 'hello world'
    expect(html_textAreaGet(ta)).toBe('hello world')
  })

  test('reflects updates to textContent', () => {
    const ta = document.createElement('textarea')
    ta.textContent = 'first'
    expect(html_textAreaGet(ta)).toBe('first')
    ta.textContent = 'second'
    expect(html_textAreaGet(ta)).toBe('second')
  })
})

describe('button?', () => {
  test('is true for a value made by button', async () => {
    expect(await runProgram(`
    (import html)
    (button? (button "Click" (lambda () #t)))
    `)).toEqual(['#t'])
  })

  test('is false for non-button values', async () => {
    expect(await runProgram(`
    (import html)
    (button? (tag "div"))
    (button? 5)
    (button? "button")
    (button? (list 1 2 3))
    `)).toEqual(['#f', '#f', '#f', '#f'])
  })
})

describe('button', () => {
  test('creates a button element with the given label', () => {
    const b = html_button('Click me', () => undefined)
    expect(html_isElement(b)).toBe(true)
    expect(html_buttonQ(b)).toBe(true)
    expect(b.textContent).toBe('Click me')
  })

  test('assigns a click handler', () => {
    const b = html_button('Click me', () => undefined)
    expect(typeof b.onclick).toBe('function')
  })

  // L.callScamperFn (src/lpm/lang.ts) now unconditionally throws, so
  // button's callback invocation can't be tested end-to-end (#248).
  test.skip('invokes the Scamper callback when clicked')
})

describe('tag', () => {
  test('creates an element with the given tag name', () => {
    const elt = html_tag('div')
    expect(elt.tagName).toBe('DIV')
    expect(elt.attributes.length).toBe(0)
    expect(elt.textContent).toBe('')
  })

  test('sets textContent from a string child', () => {
    const elt = html_tag('span', 'hello')
    expect(elt.textContent).toBe('hello')
  })

  test('appends an HTMLElement child', () => {
    const child = html_tag('span', 'inner')
    const elt = html_tag('div', child)
    expect(elt.children.length).toBe(1)
    expect(elt.children[0]).toBe(child)
  })

  test('sets attributes from a leading attribute list and keeps the remaining children', () => {
    const attrs = L.mkList(L.mkPair('id', 'my-id'), L.mkPair('class', 'my-class'))
    const elt = html_tag('a', attrs, 'link text')
    expect(elt.getAttribute('id')).toBe('my-id')
    expect(elt.getAttribute('class')).toBe('my-class')
    expect(elt.textContent).toBe('link text')
  })

  test('a non-list first argument is treated as a child, not an attribute list', () => {
    const elt = html_tag('div', 'not a list')
    expect(elt.attributes.length).toBe(0)
    expect(elt.textContent).toBe('not a list')
  })

  test('throws when an attribute value is not a string', () => {
    const attrs = L.mkList(L.mkPair('data-x', 5))
    expect(() => html_tag('div', attrs)).toThrow(/attribute value must be a string/)
  })

  test('throws when an attribute name is not a string', () => {
    const attrs = L.mkList(L.mkPair(5, 'five'))
    expect(() => html_tag('div', attrs)).toThrow(/attribute must be a string/)
  })
})

describe('tag-set-children!', () => {
  test('replaces existing children with the given ones, in order', () => {
    const elt = html_tag('div', html_tag('p', 'old'))
    const c1 = html_tag('span', 'a')
    const c2 = html_tag('span', 'b')
    html_tagSetChildren(elt, c1, c2)
    expect(elt.children.length).toBe(2)
    expect(elt.children[0]).toBe(c1)
    expect(elt.children[1]).toBe(c2)
    expect(elt.textContent).toBe('ab')
  })

  test('clears all children when given none', () => {
    const elt = html_tag('div', html_tag('p', 'old'))
    html_tagSetChildren(elt)
    expect(elt.children.length).toBe(0)
    expect(elt.textContent).toBe('')
  })

  test('throws when elt is not an HTML element', () => {
    expect(() => { html_tagSetChildren(42 as unknown as HTMLElement) }).toThrow(/expects an HTML element/)
  })

  test('throws when a child is not an HTML element', () => {
    const elt = html_tag('div')
    expect(() => { html_tagSetChildren(elt, 'not an element' as unknown as HTMLElement) })
      .toThrow(/expects all children to be HTML elements/)
  })
})

describe('on-keydown!', () => {
  test('registers a keydown listener on window', () => {
    const spy = vi.spyOn(window, 'addEventListener')
    html_onKeydown(() => undefined)
    expect(spy).toHaveBeenCalledWith('keydown', expect.any(Function))
    spy.mockRestore()
  })

  // L.callScamperFn (src/lpm/lang.ts) now unconditionally throws, so
  // on-keydown!'s callback invocation can't be tested end-to-end (#248).
  test.skip('invokes the Scamper callback with the pressed key')
})
