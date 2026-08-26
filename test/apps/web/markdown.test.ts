import { describe, expect, test } from 'vitest'
import { renderMarkdown } from '../../../src/app/web/markdown'

/** `md` rendered, as HTML, which is what a prose cell shows (#410). */
function html(md: string): string {
  const host = document.createElement('div')
  host.appendChild(renderMarkdown(md))
  return host.innerHTML
}

describe('rendering a prose cell', () => {
  test('a paragraph', () => {
    expect(html('Hello there.')).toBe('<p>Hello there.</p>')
  })

  test('headings', () => {
    expect(html('# Part One')).toBe('<h1>Part One</h1>')
    expect(html('### A step')).toBe('<h3>A step</h3>')
  })

  test('emphasis', () => {
    expect(html('A *word* and a **phrase**.')).toBe(
      '<p>A <em>word</em> and a <strong>phrase</strong>.</p>',
    )
  })

  test('an inline code span', () => {
    expect(html('Call `(square 5)` now.')).toBe(
      '<p>Call <code>(square 5)</code> now.</p>',
    )
  })

  test('a fenced code block keeps its text and drops its fences', () => {
    expect(html('```scheme\n(+ 1 2)\n```')).toBe(
      '<pre><code>(+ 1 2)</code></pre>',
    )
  })

  test('a bullet list', () => {
    expect(html('- one\n- two')).toBe('<ul><li><p>one</p></li><li><p>two</p></li></ul>')
  })

  test('a numbered list', () => {
    expect(html('1. one\n2. two')).toBe(
      '<ol><li><p>one</p></li><li><p>two</p></li></ol>',
    )
  })

  test('a rule', () => {
    expect(html('---')).toBe('<hr>')
  })

  test('a blockquote', () => {
    expect(html('> Quoted.')).toBe('<blockquote><p>Quoted.</p></blockquote>')
  })

  test('a link opens away from the notebook', () => {
    expect(html('[docs](https://scamper.cs.grinnell.edu)')).toBe(
      '<p><a href="https://scamper.cs.grinnell.edu" target="_blank" rel="noopener noreferrer">docs</a></p>',
    )
  })

  test('an image', () => {
    expect(html('![a cat](cat.png)')).toBe(
      '<p><img src="cat.png" alt="a cat"></p>',
    )
  })

  test('an escaped character is itself', () => {
    expect(html('2 \\* 3')).toBe('<p>2 * 3</p>')
  })

  test('several paragraphs', () => {
    expect(html('One.\n\nTwo.')).toBe('<p>One.</p><p>Two.</p>')
  })
})

// A prose cell is a comment out of a file that may have arrived from a lab
// handout or a shared archive. Nothing in one may become markup.
describe('markup in the source', () => {
  test('a raw tag is shown as text', () => {
    expect(html('<script>alert(1)</script>')).toBe(
      '&lt;script&gt;alert(1)&lt;/script&gt;',
    )
  })

  test('an inline tag is shown as text', () => {
    expect(html('Hello <b>there</b>.')).toBe(
      '<p>Hello &lt;b&gt;there&lt;/b&gt;.</p>',
    )
  })

  test('a script URL in a link is dropped', () => {
    expect(html('[click](javascript:alert(1))')).toBe(
      '<p><a href="" target="_blank" rel="noopener noreferrer">click</a></p>',
    )
  })

  test('a data URL in an image is dropped', () => {
    expect(html('![x](data:text/html,<script>alert(1)</script>)')).toBe(
      '<p><img src="" alt="x"></p>',
    )
  })
})
