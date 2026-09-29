import { beforeEach, describe, expect, test } from 'vitest'
import { EMBED_CLASS, readSpec, runEmbeds } from '../../src/app/web/embed/embed'
import { initialize } from '../../src/scamper'
import { byId, query } from '../dom'

await initialize()

// https://github.com/slag-plt/scamper/issues/665
//
// A widget may write its code either as a `<script type="text/scamper">` or as
// the element's own text. The second form read that text with `el.textContent`,
// and a script element's text is part of its parent's textContent -- so a
// preamble script sitting beside bare text had its source folded into the
// visible program. It was captioned in the transcript and run a second time
// there, which is why a preamble that printed anything printed it in the open.
//
// What this pins is `readSpec`'s answer to "what counts as the code", which is
// where the leak was. `runOne` was never at fault: it already runs the preamble
// into a display nothing is attached to.

/** The markup from the issue, verbatim: a preamble script beside bare text. */
const ISSUE_MARKUP = `<pre class="${EMBED_CLASS}">
<script type="text/scamper-preamble">(define numbers (list 4 1 6 3 2 10 8))</script>
(reduce - numbers)
(reduce-right - numbers)
</pre>`

/** Builds a widget from markup and returns it. */
function widget(html: string): HTMLElement {
  document.body.innerHTML = html
  return query(document.body, `.${EMBED_CLASS}`)
}

/** The transcript's rendered text, whitespace squashed so assertions read. */
function transcript(el: HTMLElement): string {
  return el.textContent.replace(/\s+/g, ' ').trim()
}

/** The source captions a widget rendered, in order. */
function captions(el: HTMLElement): string[] {
  return [...el.querySelectorAll('.scamper-transcript-source')].map((node) =>
    node.textContent.replace(/\s+/g, ' ').trim(),
  )
}

beforeEach(() => {
  document.body.innerHTML = ''
})

describe('a preamble beside bare text', () => {
  test('is not part of the code', () => {
    const spec = readSpec(widget(ISSUE_MARKUP))
    expect(spec.code).toBe('(reduce - numbers)\n(reduce-right - numbers)')
    expect(spec.code).not.toContain('define numbers')
    expect(spec.preamble).toBe('(define numbers (list 4 1 6 3 2 10 8))')
  })

  test('is not captioned in the transcript', async () => {
    const el = widget(ISSUE_MARKUP)
    await runEmbeds()

    expect(captions(el)).toEqual([
      '(reduce - numbers)',
      '(reduce-right - numbers)',
    ])
  })

  // Hidden, not dropped: the reader does not see the definition, but the code
  // they do see still resolves against it.
  test('is still in scope for the code that is shown', async () => {
    const el = widget(ISSUE_MARKUP)
    await runEmbeds()

    const text = transcript(el)
    expect(text).toContain('-26')
    expect(text).toContain('6')
    expect(text.toLowerCase()).not.toContain('error')
  })

  // The issue's own example could not show this half, because `define` prints
  // nothing: a preamble folded into the visible program was *re-executed*
  // there, so its output landed in the transcript too.
  test('does not print into the transcript', async () => {
    const el = widget(
      `<pre class="${EMBED_CLASS}">
<script type="text/scamper-preamble">(define x 1)
(+ 100 11)</script>
(+ x 1)
</pre>`,
    )
    await runEmbeds()

    const text = transcript(el)
    expect(captions(el)).toEqual(['(+ x 1)'])
    expect(text).not.toContain('(+ 100 11)')
    expect(text).not.toContain('111')
    expect(text).toContain('2')
  })

  // Between two visible forms rather than before them, which is where a fix
  // that closed up the gap the script left would misattribute output: the
  // statement ranges the transcript captions with are ranges into this very
  // string, so the blank line it leaves is harmless and the pairing holds.
  test('may sit between two forms without shifting their captions', async () => {
    const el = widget(
      `<pre class="${EMBED_CLASS}">
(display "FIRST")
<script type="text/scamper-preamble">(define x 99)</script>
(display "SECOND")
</pre>`,
    )
    expect(readSpec(el).code).toBe('(display "FIRST")\n\n(display "SECOND")')

    await runEmbeds()

    expect(captions(el)).toEqual(['(display "FIRST")', '(display "SECOND")'])
    // The captions alone would pass with the two outputs swapped; the whole
    // text in order is what pins each one to the form above it.
    expect(transcript(el)).toBe(
      '(display "FIRST")"FIRST"(display "SECOND")"SECOND"',
    )
  })

  // Nested, not a direct child: `scriptText` finds a preamble anywhere in the
  // widget, so `ownText` has to lose it from anywhere too or the two disagree.
  test('is left out even when it is nested inside a child element', () => {
    const el = widget(
      `<div class="${EMBED_CLASS}"><span><script type="text/scamper-preamble">(define z 7)</script></span>(+ z 1)</div>`,
    )
    expect(readSpec(el)).toMatchObject({
      code: '(+ z 1)',
      preamble: '(define z 7)',
    })
  })

  // Not about the preamble, but about how it is left out: reading the children
  // one at a time picks up a comment's own data, which `el.textContent` never
  // did, and an author's note would then be run as code.
  test('does not turn an html comment the author left into code', () => {
    const el = widget(
      `<pre class="${EMBED_CLASS}">
<!-- TODO explain this -->
(+ 1 2)
</pre>`,
    )
    expect(readSpec(el).code).toBe('(+ 1 2)')
  })

  // The one case where the widget's own text is empty, and so the case most
  // likely to become an error rather than an empty transcript.
  test('can be the whole widget, and still hands its environment on', async () => {
    document.body.innerHTML = `
      <div class="${EMBED_CLASS}" id="setup"><script type="text/scamper-preamble">(define x 41)</script></div>
      <div class="${EMBED_CLASS}" id="uses" data-continues>(+ x 1)</div>`
    const setup = byId('setup')
    expect(readSpec(setup).code).toBe('')

    await runEmbeds()

    expect(transcript(setup)).toBe('')
    expect(captions(setup)).toEqual([])
    expect(transcript(byId('uses'))).toContain('42')
  })
})
