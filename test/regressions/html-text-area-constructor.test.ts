import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import {
  html_isElement,
  html_textArea,
  html_textAreaQ,
} from '../../src/js/html/index.js'

// https://github.com/slag-plt/scamper/issues/260
// html_textArea built its result via `new HTMLTextAreaElement()`, an illegal
// constructor (HTML element interfaces carry [HTMLConstructor] and throw
// outside custom-element upgrade) in every environment, jsdom included. The
// fix uses document.createElement('textarea'), matching sibling html_tag /
// html_button.
describe('#260: text-area constructor', () => {
  test('html_textArea builds a text-area element without throwing', () => {
    const ta = html_textArea('my-id')
    expect(html_isElement(ta)).toBe(true)
    expect(html_textAreaQ(ta)).toBe(true)
    expect(ta.tagName).toBe('TEXTAREA')
    expect(ta.id).toBe('my-id')
  })

  test('(text-area id) runs without a runtime error', async () => {
    const out = await runProgram(`
    (import html)
    (text-area? (text-area "my-id"))
    `)
    expect(out).toEqual(['#t'])
  })
})
